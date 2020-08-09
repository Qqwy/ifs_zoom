{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}

module Interactive
  ( run
  ) where

import Pipe
import Options(CLIOptions, HasCLIOptions(..))
import qualified IFSConfig
import Input (Input, save_screenshot)
import qualified Input
import SimState(SimState(..), input, picture, should_update)
import qualified SimState

import Lens.Micro.Platform

import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Data.Array.Accelerate(Z(..), (:.)(..))
import Graphics.Gloss.Interface.IO.Game(Event)

import qualified Data.Array.Accelerate.IO.Codec.BMP as IOBMP
import qualified Data.Array.Accelerate.System.Random.MWC




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
    (SimState.initial ifs_config options random_matrix)
    SimState.drawWithGuides
    handleInput
    SimState.update

handleInput :: Event -> SimState -> IO SimState
handleInput event sim_state = do
  let input' = Input.handleEvent event (sim_state^.input)
  applyInput input' sim_state


-- | Applies the effect of the abstracted input events
-- to the current SimState, effectfully
applyInput :: Input -> SimState -> IO SimState
applyInput input' sim_state | input' == (sim_state^.input) = return sim_state
applyInput input' sim_state =
  sim_state
  |> set input input'
  |> set should_update True
  |> SimState.applyDragging
  |> SimState.applyZooming
  |> SimState.applyJumping
  |> maybeScreenshot

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

