{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive
  ( run
  ) where

import Pipe
import qualified Data.Maybe

import Options(CLIOptions, HasCLIOptions(..))

import qualified IFSConfig
import IFSConfig(transformations)


import Lib.Common (Point)
import qualified Lib
import qualified Lib.Camera
import qualified Lib.ChaosGame
import qualified Lib.Guide
import Lib.Transformation (IFS)
import qualified Lib.Transformation

import Lens.Micro.Platform

import qualified Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Data.Array.Accelerate as Accelerate
import Data.Array.Accelerate(Z(..), (:.)(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), KeyState(..))
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX

import qualified Data.Array.Accelerate.IO.Codec.BMP as IOBMP
import qualified Data.Array.Accelerate.System.Random.MWC

data Zooming = ZoomOut | ZoomIn
  deriving (Eq, Ord, Show)


data Input = Input
  { _dragging :: Maybe (Float, Float)
    -- ^ Nothing if not dragging, Just (x, y) when we are dragging,
    -- where (x, y) is the position we started to drag from
  , _translation :: (Float, Float)
  , _zooming :: Maybe Zooming
  , _save_screenshot :: Bool
  , _show_guides :: Bool
  , _show_points :: Bool
  }
  deriving (Eq, Ord, Show)

makeLenses ''Input

data SimState = SimState
  { _picture :: !Lib.RasterPicture
  , _point_cloud :: Accelerate.Acc (Accelerate.Vector Point)
  , _should_update :: Bool
  , _dimensions :: (Word, Word)
  , _camera :: Lib.Camera
  , _input :: Input
  , _transformations_list :: [IFSConfig.TransformationWithProbability]
  , _initial_camera :: Lib.Camera
  }

makeLenses ''SimState

run :: IFSConfig.IFS -> CLIOptions -> IO ()
run ifs_config options = do
  let
    width = options ^. render_width |> fromIntegral
    height = options ^. render_height |> fromIntegral
    position = (10, 10)
    window = (Gloss.InWindow "Iterated Function Systems Exploration" (width, height) position)
    samples' = options ^. samples |> fromIntegral
    paralellism' = options ^. paralellism |> fromIntegral
    n_points_per_thread = samples' `div` paralellism'

  random_matrix <- Data.Array.Accelerate.System.Random.MWC.randomArray Data.Array.Accelerate.System.Random.MWC.uniform (Z :. n_points_per_thread :. paralellism')

  Gloss.playIO
    window
    Gloss.black
    20
    (initialSimState ifs_config options random_matrix)
    drawSimStateWithHelpers
    handleInput
    updateSimState

handleInput :: Event -> SimState -> IO SimState
handleInput event sim_state = do
  let
    input' = parseInput event (sim_state^.input)

  if input' == (sim_state^.input) then
    return sim_state
  else do
    applyInput input' sim_state

-- | Transforms user actions on the keyboard + mouse
-- to a more abstract 'input' format
-- that makes sense across frames.
-- (i.e. abstracting individual events into things like dragging and toggling.)
parseInput :: Event -> Input -> Input
parseInput event input =
  case event of
    EventKey (MouseButton LeftButton) Up _ _ ->
      input{_dragging = Nothing}
    EventKey (MouseButton LeftButton) Down _ pos ->
      input{_dragging = Just pos, _translation = (0, 0)}
    EventMotion (x, y) ->
      case input^.dragging of
        Nothing ->
          input
        Just (x0, y0) ->
          input
          |> set dragging (Just (x, y))
          |> (translation._1) +~ (x - x0)
          |> (translation._2) +~ (y - y0)
    EventKey (MouseButton WheelUp) _ _ _ ->
      input{_zooming = Just ZoomIn}
    EventKey (MouseButton WheelDown) _ _ _ ->
      input{_zooming = Just ZoomOut}
    EventKey (Char 's') Down _ _ ->
      input{_save_screenshot = True}
    EventKey (Char 'p') Down _ _ ->
      input
      |> toggle show_points
    EventKey (Char 'g') Down _ _ ->
      input
      |> toggle show_guides
    _ ->
      input
  where
    toggle attribute = over attribute not

-- | Applies the effect of the abstracted input events
-- to the current SimState, effectfully
applyInput :: Input -> SimState -> IO SimState
applyInput input sim_state = do
  sim_state
  |> return . fillInput input
  >>= return . applyDragging
  >>= return . applyZooming
  >>= maybeScreenshot
  >>= return . shouldUpdate
  where
    fillInput input sim_state = sim_state{_input = input}
    shouldUpdate sim_state = sim_state{_should_update = True}

applyDragging :: SimState -> SimState
applyDragging sim_state =
  let
    (screen_x, screen_y) = sim_state^.dimensions |> over both fromIntegral
    (tx, ty) = sim_state^.input.translation
    (unit_x, unit_y) = (tx / screen_x, ty / screen_y)
  in
    sim_state
    |> over camera (Lib.Camera.translate unit_x (-unit_y))
    |> set (input.translation) (0, 0)

applyZooming :: SimState -> SimState
applyZooming sim_state =
  let
    speed = 0.025
  in
    case sim_state^.input.zooming of
      Nothing ->
        sim_state
      Just ZoomIn ->
        sim_state
        |> over camera (Lib.Camera.scale (1 + speed))
        |> set (input.zooming) Nothing
      Just ZoomOut ->
        sim_state
        |> over camera (Lib.Camera.scale (1 - speed))
        |> set (input.zooming) Nothing

maybeScreenshot :: SimState -> IO SimState
maybeScreenshot sim_state =
  case sim_state^.input.save_screenshot of
    False ->
      return sim_state
    True -> do
      IOBMP.writeImageToBMP "example_picture.bmp" (sim_state^.picture)
      sim_state
        |> set (input.save_screenshot) False
        |> return

-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
updateSimState :: Float -> SimState -> IO SimState
updateSimState _time_elapsed sim_state =
  case sim_state^.should_update of
    False ->
      return sim_state
    True -> do
      let
        new_sim_state =
          sim_state
          |> set picture (renderSimState sim_state)
          |> set should_update False

      putStrLn (show (sim_state^.camera))
      -- let cam = ((sim_state^.camera) |> Lib.Camera.withInitialCamera (sim_state^.initial_camera) )
      let cam = sim_state^.camera
      let res = map (Lib.Guide.isCameraInsideTransformation cam (sim_state^.initial_camera)) (extractTransformations (sim_state^.transformations_list))
      -- let res = map (Lib.Camera.isCameraInsideTransformation cam) (extractTransformations (sim_state^.transformations_list))
      putStrLn (show res)

      return new_sim_state

drawSimStateWithHelpers :: SimState -> IO Gloss.Picture
drawSimStateWithHelpers sim_state =
  [sim_state_picture, guides_picture]
  |> Graphics.Gloss.Data.Picture.pictures
  |> return
  where
    sim_state_picture =
      if sim_state^.input.show_points
      then drawSimState sim_state
      else Graphics.Gloss.Data.Picture.blank
    guides_picture =
      if sim_state^.input.show_guides
      then Lib.Guide.drawGuides (sim_state^.camera) (sim_state^.initial_camera) (sim_state^.dimensions) (extractTransformations (sim_state^.transformations_list))
      else Graphics.Gloss.Data.Picture.blank

-- TODO refactor this
extractTransformations transformations_list =
  transformations_list
  |> map IFSConfig.transformationWithProbabilityToSixtuplePair
  |> map Lib.Transformation.fromSixtuplePair
  |> map fst

-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
drawSimState :: SimState -> Gloss.Picture
drawSimState sim_state =
  sim_state^.picture
  |> (\picture -> Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray picture True)
  |> Graphics.Gloss.Data.Picture.scale 1 (-1) -- Gloss renders pictures upside-down https://github.com/tmcdonell/gloss-accelerate/issues/2

renderSimState :: SimState -> Lib.RasterPicture
renderSimState sim_state =
  sim_state^.point_cloud
  |> Lib.naivePointCloudToPicture camera' width height
  |> Data.Array.Accelerate.LLVM.PTX.run
  where
    (width, height) =
      sim_state^.dimensions
      |> over both (\val -> val |> fromIntegral |> Accelerate.unit)
    camera' =
      -- ((Linear.Matrix.inv33 (sim_state^.initial_camera)) Linear.Matrix.!*! (sim_state^.camera))
      sim_state^.camera
      |> Lib.Camera.withInitial (sim_state^.initial_camera)
      |> Accelerate.lift
      |> Accelerate.unit

initialSimState :: IFSConfig.IFS -> CLIOptions -> Accelerate.Array Accelerate.DIM2 Accelerate.Word64 -> SimState
initialSimState ifs_config options random_matrix =
  SimState
  { _picture = Accelerate.fromList (Z :. 0 :. 0) []
  , _should_update = True
  , _point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism' (Accelerate.use random_matrix)
  , _dimensions = (picture_width, picture_height)
  -- , _camera = Lib.Camera.defaultCamera (ifs_config |> IFSConfig.initialCamera |> IFSConfig.transformationToSixtuple)
  , _camera = Lib.Camera.identity
  , _input = initialInput
  , _transformations_list = transformations_list
  , _initial_camera = Lib.Camera.fromSixtuple (ifs_config |> IFSConfig.initialCamera |> IFSConfig.transformationToSixtuple)
  }
  where
    seed' = options ^. seed |> Data.Maybe.fromMaybe 0
    samples' = options ^. samples |> fromIntegral
    paralellism' = options ^. paralellism |> fromIntegral
    n_points_per_thread = samples' `div` paralellism'
    picture_width = options ^. render_width |> fromIntegral
    picture_height = options ^. render_height |> fromIntegral
    transformations_list = ifs_config |> IFSConfig.transformations
    transformations = buildTransformations transformations_list

initialInput :: Input
initialInput =
  Input
  { _dragging = Nothing
  , _zooming = Nothing
  , _translation = (0, 0)
  , _save_screenshot = False
  , _show_guides = False
  , _show_points = True
  }

buildTransformations :: [IFSConfig.TransformationWithProbability] -> Accelerate.Acc IFS
buildTransformations transformations_list =
  transformations_list
  |> map IFSConfig.transformationWithProbabilityToSixtuplePair
  |> Accelerate.fromList (Z :. (length transformations_list))
  |> Accelerate.use
  |> Accelerate.map Lib.Transformation.fromSixtuplePairGPU
