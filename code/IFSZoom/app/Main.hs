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
import Options(CLIOptions, HasCLIOptions(..))

import qualified Interactive

import Prelude                                                      as P
import qualified System.Random
import Lens.Micro.Platform

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
main =
  -- if ((options^.samples) P.< (options^.paralellism)) then do
  --   putStrLn "Error. `samples` should be larger than `paralellism`."
  -- else
  Options.parseCommandLineOptions
  >>= maybeSeedRNG
  >>= runProgram

maybeSeedRNG :: Options.CLIOptions -> IO Options.CLIOptions
maybeSeedRNG options@(Options.CLIOptions {Options._seed = 0}) = do
  auto_seed <- System.Random.getStdRandom System.Random.random
  putStrLn ("Using seed " P.++ (show auto_seed))
  return (options {Options._seed = auto_seed})

maybeSeedRNG options = return options

runProgram :: Options.CLIOptions -> IO ()
runProgram options = do
  let
    -- Barnsley Fern:
    transformations =
      -- [ ((0,0,0,0.16,0.0, 0), 0.01)
      -- , ((0.85,0.04, -0.04, 0.85,  0, 1.60), 0.85)
      -- , ((0.20, -0.26, 0.23, 0.22, 0, 1.60), 0.07)
      -- , ((-0.15, 0.28, 0.26, 0.24, 0, 0.44), 0.07)
      -- ]
    -- Sierpińsky Triangle:
      [ ((0.5, 0, 0, 0.5, 0, 0), 1/3)
      , ((0.5, 0, 0, 0.5, 0.5, 0), 1/3)
      , ((0.5, 0, 0, 0.5, 0.25, (sqrt 3) / 4), 1/3)
      ]

  Interactive.run transformations options
