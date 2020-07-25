{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive
  ( run
  ) where

import Pipe

import qualified Options
import Options(CLIOptions, HasCLIOptions(..))

import qualified IFSConfig
import IFSConfig(transformations)


import Lib.Common
import qualified Lib
import qualified Lib.ChaosGame
import qualified Lib.Camera

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
  , _tx :: Float
  , _ty :: Float
  , _zooming :: Maybe Zooming
  , _save_screenshot :: Bool
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
  , _transformations :: [IFSConfig.TransformationWithProbability]
  }

makeLenses ''SimState

type IFSTransformation =
  ((Float, Float, Float, Float, Float, Float), Float)

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
    input' = handleInput' event (sim_state^.input)

  if input' == (sim_state^.input) then
    return sim_state
  else do
    applyInput input' sim_state

handleInput' :: Event -> Input -> Input
handleInput' event input =
  case event of
    EventKey (MouseButton LeftButton) Up _ _ ->
      input{_dragging = Nothing}
    EventKey (MouseButton LeftButton) Down _ pos ->
      input{_dragging = Just pos, _tx = 0, _ty = 0}
    EventMotion (x, y) ->
      case input^.dragging of
        Nothing ->
          input
        Just (x0, y0) ->
          input
          |> set dragging (Just (x, y))
          |> tx +~ (x - x0)
          |> ty +~ (y - y0)
    EventKey (MouseButton WheelUp) _ _ _ ->
      input{_zooming = Just ZoomIn}
    EventKey (MouseButton WheelDown) _ _ _ ->
      input{_zooming = Just ZoomOut}
    EventKey (Char 's') Down _ _ ->
      input{_save_screenshot = True}
    _ ->
      input

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
applyDragging sim_state@SimState{_input = input@Input{_tx, _ty}, _camera, _dimensions = (screen_width, screen_height)} =
    sim_state
    { _camera = Lib.Camera.translateCamera unit_x unit_y _camera
    , _input = input{ _tx = 0, _ty = 0}
    }
  where
    unit_x = _tx / (fromIntegral screen_width)
    unit_y = -_ty / (fromIntegral screen_height)

applyZooming :: SimState -> SimState
applyZooming sim_state =
  case sim_state^.input.zooming of
    Nothing ->
      sim_state
    Just ZoomIn ->
      sim_state
      |> over camera (Lib.Camera.scaleCamera 1.025)
      |> set (input.zooming) Nothing
    Just ZoomOut ->
      sim_state
      |> over camera (Lib.Camera.scaleCamera 0.975)
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

      return new_sim_state

drawSimStateWithHelpers :: SimState -> IO Gloss.Picture
drawSimStateWithHelpers sim_state =
  Graphics.Gloss.Data.Picture.pictures
  [ drawSimState sim_state,
    drawGuides sim_state
  ]
  |> return

drawGuides :: SimState -> Gloss.Picture
drawGuides _sim_state = Graphics.Gloss.Data.Picture.blank

drawGuide (x, y, w, h) camera =
  Graphics.Gloss.Data.Picture.blank

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
      sim_state^.camera
      |> Accelerate.lift
      |> Accelerate.unit

initialSimState :: IFSConfig.IFS -> CLIOptions -> Accelerate.Array Accelerate.DIM2 Accelerate.Word64 -> SimState
initialSimState ifs_config options random_matrix =
  SimState
  { _picture = Accelerate.fromList (Z :. 0 :. 0) []
  , _should_update = True
  , _point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism' (Accelerate.use random_matrix)
  , _dimensions = (picture_width, picture_height)
  , _camera = Lib.Camera.defaultCamera (ifs_config |> IFSConfig.initialCamera |> IFSConfig.transformationToSixtuple)
  , _input = initialInput
  , _transformations = transformations_list
  }
  where
    seed' = options ^. seed
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
  , _tx = 0
  , _ty = 0
  , _save_screenshot = False
  }

buildTransformations :: [IFSConfig.TransformationWithProbability] -> Accelerate.Acc IFS
buildTransformations transformations_list =
  transformations_list
  |> map IFSConfig.transformationWithProbabilityToSixtuplePair
  |> Accelerate.fromList (Z :. (length transformations_list))
  |> Accelerate.use
  |> Accelerate.map (Lib.ChaosGame.transformationProbabilityFromSixtuplePair)
