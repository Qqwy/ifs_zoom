{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Interactive
  ( run
  ) where

import Pipe

import qualified Options
import Options(CLIOptions, HasCLIOptions(..))

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
  { _dragging :: Maybe (Float, Float) -- ^ Nothing if not dragging, Just ((x0, y0), (x, y)) if dragging, where (x0, y0) is the position we started the motion at.
  , _tx :: Float
  , _ty :: Float
  , _zooming :: Maybe Zooming
  , _saveScreenshot :: Bool
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
  }

makeLenses ''SimState

type IFSTransformation =
  ((Float, Float, Float, Float, Float, Float), Float)

run :: [IFSTransformation] -> CLIOptions -> IO ()
run transformations options = do
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
    (initialSimState transformations options random_matrix)
    drawSimState
    handleInput
    updateSimState


handleInput :: Event -> SimState -> IO SimState
handleInput event sim_state = do
  -- putStrLn (show event)
  let
    input' = handleInput' event (sim_state ^. input)

  -- putStrLn (show input')

  if input' == (sim_state ^.input) then
    return sim_state
  else do
    applyInput input' sim_state

handleInput' :: Event -> Input -> Input
handleInput' event input =
  case (event, input) of
    (EventKey (MouseButton LeftButton) Up _ _, _) ->
      input{_dragging = Nothing}
    (EventKey (MouseButton LeftButton) Down _ pos, _) ->
      input{_dragging = Just pos, _tx = 0, _ty = 0}
    (EventMotion (_x, _y), Input{_dragging = Nothing}) ->
      input
    (EventMotion (x, y), Input{_dragging = Just (x0, y0), _tx, _ty}) ->
      input{_dragging = Just (x, y)
           , _tx = _tx + x'
           , _ty = _ty + y'
           }
      where
        x' = x - x0
        y' = y - y0
    (EventKey (MouseButton WheelUp) _ _ _, _) ->
      input{_zooming = Just ZoomIn}
    (EventKey (MouseButton WheelDown) _ _ _, _) ->
      input{_zooming = Just ZoomOut}
    (EventKey (Char 's') Down _ _, Input{}) ->
      input{_saveScreenshot = True}
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
-- applyZooming sim_state@SimState{input = Input{zooming = Nothing}} =
--   sim_state
-- applyZooming sim_state@SimState{input = input@Input{zooming = Just ZoomIn}, camera} =
--   sim_state{camera = Lib.Camera.scaleCamera 1.025 camera, input = input{zooming = Nothing}}
-- applyZooming sim_state@SimState{input = input@Input{zooming = Just ZoomOut}, camera} =
--   sim_state{camera = Lib.Camera.scaleCamera 0.975 camera, input = input{zooming = Nothing}}
applyZooming sim_state =
  sim_state
  |> over camera maybeZoomCam
  where
    maybeZoomCam cam =
      case sim_state^.input.zooming of
        Nothing       -> id cam
        Just ZoomIn   -> Lib.Camera.scaleCamera 1.025 cam
        Just ZoomOut  -> Lib.Camera.scaleCamera 0.975 cam


maybeScreenshot :: SimState -> IO SimState
maybeScreenshot sim_state@SimState{_input = Input{_saveScreenshot = False}} =
  return sim_state
maybeScreenshot sim_state@SimState{_input = input@Input{_saveScreenshot = True}, _picture} = do
  IOBMP.writeImageToBMP "example_picture.bmp" _picture
  return sim_state{_input = input{_saveScreenshot = False}}

-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
updateSimState :: Float -> SimState -> IO SimState
updateSimState _ sim_state@SimState{_should_update} =
  case _should_update of
    False -> return sim_state
    True -> do
      let
        new_picture = (renderSimState sim_state)
          -- |> applyInverseViewport viewport
        new_sim_state = sim_state { _should_update = False
                                  , _picture = new_picture
                  -- , camera = viewportToCamera sim_state viewport
                  }

      putStrLn (show (sim_state ^. camera))
      -- IOBMP.writeImageToBMP "example_picture.bmp" new_picture

      return new_sim_state

-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
drawSimState :: SimState -> IO Gloss.Picture
drawSimState sim_state =
  sim_state
  ^. picture
  |> (\picture -> Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray picture True)
  |> Graphics.Gloss.Data.Picture.scale 1 (-1) -- Gloss renders pictures upside-down https://github.com/tmcdonell/gloss-accelerate/issues/2
  |> return

renderSimState :: SimState -> Lib.RasterPicture
renderSimState sim_state =
  sim_state^.point_cloud
  |> Lib.naivePointCloudToPicture camera' width' height'
  |> Data.Array.Accelerate.LLVM.PTX.run
  where
    (width, height) = sim_state^.dimensions
    camera' = sim_state^.camera |> Accelerate.lift |> Accelerate.unit
    width'  = width  |> fromIntegral    |> Accelerate.unit
    height' = height |> fromIntegral    |> Accelerate.unit


-- reactToUserInput :: Gloss.Event -> SimState -> IO SimState
-- reactToUserInput event sim_state =
--   case event of
--     -- EventKey (Char 'w') s _ _ -> toggle zoom 0.975 s sim_state
--     -- EventKey (Char 'a') s _ _ -> toggle zoom 1.025 s sim_state
--     _ -> return sim_state
--     where
--       -- toggle fun val Gloss.Down sim_state = sim_state |> set fun (Just val) |> dirty |> return
--       -- toggle fun _   Gloss.Up   sim_state = sim_state |> set fun Nothing |> return
--       -- dirty sim_state = sim_state { shouldUpdate = True }

initialSimState :: [IFSTransformation] -> CLIOptions -> Accelerate.Array Accelerate.DIM2 Accelerate.Word64 -> SimState
initialSimState transformations_list options random_matrix =
  SimState
  { _picture = Accelerate.fromList (Z :. 0 :. 0) []
  , _should_update = True
  , _point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism' (Accelerate.use random_matrix)
  , _dimensions = (picture_width, picture_height)
  , _camera = Lib.Camera.defaultCamera
  , _input = initialInput
  }
  where
    seed' = options ^. Options.seed
    samples' = options ^. Options.samples |> fromIntegral
    paralellism' = options ^. Options.paralellism |> fromIntegral
    n_points_per_thread = samples' `div` paralellism'
    picture_width = options ^. Options.render_width |> fromIntegral
    picture_height = options ^. Options.render_height |> fromIntegral

    transformations = buildTransformations transformations_list

initialInput :: Input
initialInput =
  Input
  { _dragging = Nothing
  , _zooming = Nothing
  , _tx = 0
  , _ty = 0
  , _saveScreenshot = False
  }

buildTransformations :: [IFSTransformation] -> Accelerate.Acc IFS
buildTransformations transformations_list =
  transformations_list
  |> Accelerate.fromList (Z :. (length transformations_list))
  |> Accelerate.use
  |> Accelerate.map (Lib.ChaosGame.transformationProbabilityFromSixtuplePair)

-- viewportToCamera :: SimState -> Gloss.ViewPort -> Lib.Camera
-- viewportToCamera SimState{dimensions = (width, height)} (Gloss.ViewPort (tx, ty) rotate scale) =
--   Lib.Camera.defaultCamera
--   |> Lib.Camera.scaleCamera scale
--   |> Lib.Camera.translateCamera horizontal vertical
--   where
--     horizontal = tx / (fromIntegral width)
--     vertical = ty / (fromIntegral height)
