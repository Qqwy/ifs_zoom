{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns #-}

module Interactive
  ( run
  ) where

import Pipe

import qualified Options
import Options(CLIOptions)

import Lib.Common
import qualified Lib
import qualified Lib.ChaosGame
import qualified Lib.Camera

import qualified Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Data.Array.Accelerate as Accelerate
import Data.Array.Accelerate(Z(..), (:.)(..))
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..), MouseButton(..), KeyState(..))
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX

import qualified Data.Array.Accelerate.IO.Codec.BMP as IOBMP

data Zooming = ZoomOut | ZoomIn
  deriving (Eq, Ord, Show)

data Input = Input
  { dragging :: Maybe (Float, Float) -- ^ Nothing if not dragging, Just ((x0, y0), (x, y)) if dragging, where (x0, y0) is the position we started the motion at.
  , tx :: Float
  , ty :: Float
  , zooming :: Maybe Zooming
  , saveScreenshot :: Bool
  }
  deriving (Eq, Ord, Show)

data SimState = SimState
  { picture :: !Lib.RasterPicture
  , point_cloud :: Accelerate.Acc (Accelerate.Vector Point)
  , should_update :: Bool
  , dimensions :: (Word, Word)
  , camera :: Lib.Camera
  , input :: Input
  }

type IFSTransformation =
  ((Float, Float, Float, Float, Float, Float), Float)

run :: [IFSTransformation] -> CLIOptions -> IO ()
run transformations options = do
  let
    width = options |> Options.render_width |> fromIntegral
    height = options |> Options.render_height |> fromIntegral
    position = (10, 10)
    window = (Gloss.InWindow "Iterated Function Systems Exploration" (width, height) position)

  Gloss.playIO
    window
    Gloss.black
    20
    (initialSimState transformations options)
    drawSimState
    handleInput
    updateSimState


handleInput :: Event -> SimState -> IO SimState
handleInput event state@SimState{input} = do
  -- putStrLn (show event)
  let
    input' = handleInput' event input

  -- putStrLn (show input')

  if input' == input then
    return state
  else do
    applyInput input' state

handleInput' :: Event -> Input -> Input
handleInput' event input =
  case (event, input) of
    (EventKey (MouseButton LeftButton) Up _ _, _) ->
      input{dragging = Nothing}
    (EventKey (MouseButton LeftButton) Down _ pos, _) ->
      input{dragging = Just pos, tx = 0, ty = 0}
    (EventMotion (_x, _y), Input{dragging = Nothing}) ->
      input
    (EventMotion (x, y), Input{dragging = Just (x0, y0), tx, ty}) ->
      input{dragging = Just (x, y)
           , tx = tx + x'
           , ty = ty + y'
           }
      where
        x' = x - x0
        y' = y - y0
    (EventKey (MouseButton WheelUp) _ _ _, _) ->
      input{zooming = Just ZoomIn}
    (EventKey (MouseButton WheelDown) _ _ _, _) ->
      input{zooming = Just ZoomOut}
    (EventKey (Char 's') Down _ _, Input{}) ->
      input{saveScreenshot = True}
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
    fillInput input sim_state = sim_state{input = input}
    shouldUpdate sim_state = sim_state{should_update = True}

applyDragging :: SimState -> SimState
applyDragging sim_state@SimState{input = input@Input{tx, ty}, camera, dimensions = (screen_width, screen_height)} =
    sim_state
    { camera = Lib.Camera.translateCamera unit_x unit_y camera
    , input = input{ tx = 0, ty = 0}
    }
  where
    unit_x = tx / (fromIntegral screen_width)
    unit_y = -ty / (fromIntegral screen_height)

applyZooming :: SimState -> SimState
applyZooming sim_state@SimState{input = Input{zooming = Nothing}} =
  sim_state
applyZooming sim_state@SimState{input = input@Input{zooming = Just ZoomIn}, camera} =
  sim_state{camera = Lib.Camera.scaleCamera 1.025 camera, input = input{zooming = Nothing}}
applyZooming sim_state@SimState{input = input@Input{zooming = Just ZoomOut}, camera} =
  sim_state{camera = Lib.Camera.scaleCamera 0.975 camera, input = input{zooming = Nothing}}


maybeScreenshot :: SimState -> IO SimState
maybeScreenshot sim_state@SimState{input = Input{saveScreenshot = False}} =
  return sim_state
maybeScreenshot sim_state@SimState{input = input@Input{saveScreenshot = True}, picture} = do
  IOBMP.writeImageToBMP "example_picture.bmp" picture
  return sim_state{input = input{saveScreenshot = False}}

-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
updateSimState :: Float -> SimState -> IO SimState
updateSimState _ sim_state@SimState{should_update} =
  case should_update of
    False -> return sim_state
    True -> do
      let
        new_picture = (renderSimState sim_state)
          -- |> applyInverseViewport viewport
        new_sim_state = sim_state { should_update = False
                                  , picture = new_picture
                  -- , camera = viewportToCamera sim_state viewport
                  }

      putStrLn (show (camera sim_state))
      -- IOBMP.writeImageToBMP "example_picture.bmp" new_picture

      return new_sim_state

-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
drawSimState :: SimState -> IO Gloss.Picture
drawSimState sim_state =
  sim_state
  |> picture
  |> (\picture -> Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray picture True)
  |> Graphics.Gloss.Data.Picture.scale 1 (-1) -- Gloss renders pictures upside-down https://github.com/tmcdonell/gloss-accelerate/issues/2
  |> return

renderSimState :: SimState -> Lib.RasterPicture
renderSimState SimState{point_cloud, dimensions, camera} =
  point_cloud
  |> Lib.naivePointCloudToPicture camera' width' height'
  |> Data.Array.Accelerate.LLVM.PTX.run
  where
    (width, height) = dimensions
    camera' = camera |> Accelerate.lift |> Accelerate.unit
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

initialSimState :: [IFSTransformation] -> CLIOptions -> SimState
initialSimState transformations_list options =
  SimState
  { picture = Accelerate.fromList (Z :. 0 :. 0) []
  , should_update = True
  , point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism seed
  , dimensions = (picture_width, picture_height)
  , camera = Lib.Camera.defaultCamera
  , input = initialInput
  }
  where
    seed = options |> Options.seed
    samples = options |> Options.samples |> fromIntegral
    paralellism = options |> Options.paralellism |> fromIntegral
    n_points_per_thread = samples `div` paralellism
    picture_width = options |> Options.render_width |> fromIntegral
    picture_height = options |> Options.render_height |> fromIntegral

    transformations = buildTransformations transformations_list

initialInput :: Input
initialInput =
  Input
  { dragging = Nothing
  , zooming = Nothing
  , tx = 0
  , ty = 0
  , saveScreenshot = False
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
