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
import qualified Graphics.Gloss.Interface.IO.Simulate as Gloss
import qualified Data.Array.Accelerate as Accelerate
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..))
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX

data SimState = SimState
  { picture :: !Gloss.Picture
  , point_cloud :: Accelerate.Acc (Accelerate.Vector Point)
  , should_update :: Bool
  , dimensions :: (Word, Word)
  , current_viewport :: Gloss.ViewPort
  , camera :: Lib.Camera
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

  Gloss.simulateIO
    window
    Gloss.black
    20
    (initialSimState transformations options)
    drawSimState
    updateSimState

-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
updateSimState :: Gloss.ViewPort -> Float -> SimState -> IO SimState
updateSimState viewport _ sim_state@SimState{current_viewport} =
  if areViewportsEqual viewport current_viewport then
    return sim_state
  else do
    let
      new_picture = Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray (renderSimState sim_state) True
        -- |> applyInverseViewport viewport
      new_sim_state = sim_state { should_update = False
                , picture = new_picture
                , camera = viewportToCamera sim_state viewport
                , current_viewport = viewport
                }
      Gloss.ViewPort a b c = viewport

    putStrLn (show (a, b, c))
    return new_sim_state

-- Circumventing the missing `Eq` instance for Gloss.Viewport
areViewportsEqual :: Gloss.ViewPort -> Gloss.ViewPort -> Bool
areViewportsEqual (Gloss.ViewPort a b c) (Gloss.ViewPort d e f) =
  (a, b, c) == (d, e, f)

-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
drawSimState :: SimState -> IO Gloss.Picture
drawSimState sim_state =
  sim_state
  |> picture
  |> Graphics.Gloss.Data.Picture.scale 1 (-1) -- Gloss renders pictures upside-down https://github.com/tmcdonell/gloss-accelerate/issues/2
  |> return

applyInverseViewport (Gloss.ViewPort (tx, ty) rotation scale) picture =
  picture
  |> Graphics.Gloss.Data.Picture.scale (recip scale) (recip scale)
  |> Graphics.Gloss.Data.Picture.rotate (-rotation)
  |> Graphics.Gloss.Data.Picture.translate (-tx) (-ty)


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
  { picture = Gloss.blank
  , should_update = True
  , point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism seed
  , dimensions = (picture_width, picture_height)
  , camera = Lib.Camera.defaultCamera
  , current_viewport = Gloss.ViewPort (0, 0) 0 0
  }
  where
    seed = options |> Options.seed
    samples = options |> Options.samples |> fromIntegral
    paralellism = options |> Options.paralellism |> fromIntegral
    n_points_per_thread = samples `div` paralellism
    picture_width = options |> Options.render_width |> fromIntegral
    picture_height = options |> Options.render_height |> fromIntegral

    transformations = buildTransformations transformations_list


buildTransformations :: [IFSTransformation] -> Accelerate.Acc IFS
buildTransformations transformations_list =
  transformations_list
  |> Accelerate.fromList (Accelerate.Z Accelerate.:. (length transformations_list))
  |> Accelerate.use
  |> Accelerate.map (Lib.ChaosGame.transformationProbabilityFromSixtuplePair)

viewportToCamera :: SimState -> Gloss.ViewPort -> Lib.Camera
viewportToCamera SimState{dimensions = (width, height)} (Gloss.ViewPort (tx, ty) rotate scale) =
  Lib.Camera.defaultCamera
  |> Lib.Camera.scaleCamera scale
  |> Lib.Camera.translateCamera horizontal vertical
  where
    horizontal = tx / (fromIntegral width)
    vertical = ty / (fromIntegral height)
