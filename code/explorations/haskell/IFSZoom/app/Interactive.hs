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

import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Data.Array.Accelerate as Accelerate
import Graphics.Gloss.Interface.IO.Game(Event(..), Key(..))
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX

data SimState = SimState
  { picture :: !Gloss.Picture
  , point_cloud :: Accelerate.Acc (Accelerate.Vector Point)
  , should_update :: Bool
  , dimensions :: (Word, Word)
  , camera :: Lib.Camera.Camera
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
    Gloss.red
    20
    (initialSimState transformations options)
    drawSimState
    reactToUserInput
    updateSimState

-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
updateSimState :: Float -> SimState -> IO SimState
updateSimState _ sim_state@SimState{should_update} =
  case should_update of
    False ->
      return sim_state
    True  ->
      let
        new_picture = Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray (renderSimState sim_state) True
      in
        sim_state { should_update = False
                  , picture = new_picture
                  }
        |> return

-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
drawSimState :: SimState -> IO Gloss.Picture
drawSimState sim_state = return (picture sim_state)


renderSimState SimState{point_cloud, dimensions, camera} =
  point_cloud
  |> Lib.naivePointCloudToPicture (Accelerate.use camera) (Accelerate.use (fromIntegral width)) (Accelerate.use (fromIntegral height))
  |> Data.Array.Accelerate.LLVM.PTX.run
  where
    (width, height) = dimensions


reactToUserInput :: Gloss.Event -> SimState -> IO SimState
reactToUserInput event sim_state =
  case event of
    -- EventKey (Char 'w') s _ _ -> toggle zoom 0.975 s sim_state
    -- EventKey (Char 'a') s _ _ -> toggle zoom 1.025 s sim_state
    _ -> return sim_state
    where
      -- toggle fun val Gloss.Down sim_state = sim_state |> set fun (Just val) |> dirty |> return
      -- toggle fun _   Gloss.Up   sim_state = sim_state |> set fun Nothing |> return
      -- dirty sim_state = sim_state { shouldUpdate = True }

initialSimState :: [IFSTransformation] -> CLIOptions -> SimState
initialSimState transformations_list options =
  SimState
  { picture = Gloss.blank
  , should_update = False
  , point_cloud = Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism seed
  , dimensions = (picture_width, picture_height)
  }
  where
    seed = options |> Options.seed
    samples = options |> Options.samples |> fromIntegral
    paralellism = options |> Options.paralellism |> fromIntegral
    n_points_per_thread = samples `div` paralellism
    picture_width = options |> Options.render_width |> fromIntegral
    picture_height = options |> Options.render_height |> fromIntegral

    transformations = buildTransformations transformations_list


buildTransformations transformations_list =
  transformations_list
  |> Accelerate.fromList (Accelerate.Z Accelerate.:. (length transformations_list))
  |> Accelerate.use
  |> Accelerate.map (Lib.ChaosGame.transformationProbabilityFromSixtuplePair)
