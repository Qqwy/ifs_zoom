{-# LANGUAGE CPP #-}
-- |
-- Module      : Main
-- Copyright   : [2020] Wiebe-Marten Wijnja
-- License     : BSD3
--
-- Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Pipe
import qualified Lib.ChaosGame
import qualified Lib.Picture
import qualified Lib.Camera
import qualified Options

-- import Text.Printf
import Prelude                                                      as P
import qualified System.Random

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Interpreter                            as Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.Native                            as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import Data.Array.Accelerate.LLVM.PTX                               as PTX
#endif

import qualified Graphics.Gloss
import qualified Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.IO.Codec.BMP as IOBMP



main :: IO ()
main = do
  options <- Options.parseCommandLineOptions

  if ((Options.samples options) P.< (Options.paralellism options)) then do
    putStrLn "Error. `samples` should be larger than `paralellism`."
  else
    options
    |> maybeSeedRNG
    >>= runChaosGame

maybeSeedRNG :: Options.CLIOptions -> IO Options.CLIOptions
maybeSeedRNG options@(Options.CLIOptions {Options.seed = 0}) = do
  auto_seed <- System.Random.getStdRandom System.Random.random
  putStrLn ("Using seed " P.++ (show auto_seed))
  return (options {Options.seed = auto_seed})

maybeSeedRNG options = return options

runChaosGame :: Options.CLIOptions -> IO ()
runChaosGame options = do
  let
    -- transformations =
    --   [ ((0.5, 0, 0, 0.5, 0, 0), 1/3)
    --   , ((0.5, 0, 0, 0.5, 0.5, 0), 1/3)
    --   , ((0.5, 0, 0, 0.5, 0.25, (sqrt 3) / 4), 1/3)
    --   ]
    --   |> fromList (Z :. 3)
    transformations =
      [ ((0,0,0,0.16,0.0, 0), 0.01)
      , ((0.85,0.04, -0.04, 0.85,  0, 1.60), 0.85)
      , ((0.20, -0.26, 0.23, 0.22, 0, 1.60), 0.07)
      , ((-0.15, 0.28, 0.26, 0.24, 0, 0.44), 0.07)
      ]
      |> fromList (Z :. 4)
      |> use
      |> A.map (Lib.ChaosGame.transformationProbabilityFromSixtuplePair)
    seed = options |> Options.seed
    samples = options |> Options.samples |> P.fromIntegral
    paralellism = options |> Options.paralellism |> P.fromIntegral
    n_points_per_thread = samples `div` paralellism
    picture_width = options |> Options.render_width |> P.fromIntegral
    picture_height = options |> Options.render_height |> P.fromIntegral
    camera =
      (((recip 11), 0, 0, -(recip 11), 0.5, 1) :: (Float, Float, Float, Float, Float, Float))
      |> lift
      |> Lib.Camera.cameraFromSixtuple
    program1 =
      seed
      |> Lib.ChaosGame.chaosGame transformations n_points_per_thread paralellism
      -- |> Debug.Trace.traceShowId
      -- |> Lib.Sort.sortPoints
      -- use arr
    -- result = PTX.run program1
    program2 =
      -- (use result)
      program1
      |> Lib.Picture.naivePointCloudToPicture camera picture_width picture_height
    picture =
      PTX.run program2

  -- printf "program1: %s\n" (show program1)
  -- printf "program2: %s\n" (show program2)
  -- printf "result: %s\n" (result |> A.toList |> show)
  -- printf "output (first 100 elements): %s\n" (result |> A.toList |> show)

  picture |> IOBMP.writeImageToBMP "example_picture.bmp"

  let
    -- mycircle = Circle 80 |> Graphics.Gloss.Color white
    width = options |> Options.render_width |> P.fromIntegral
    height = options |> Options.render_height |> P.fromIntegral
    position = (10, 10)
    window = (Graphics.Gloss.InWindow "Iterated Function Systems Exploration" (width, height) position)

  (Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray picture True
   |> Graphics.Gloss.Data.Picture.scale 1 (-1)
   |> Graphics.Gloss.display window Graphics.Gloss.black)
