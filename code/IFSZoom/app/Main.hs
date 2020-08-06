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

import qualified Options
import Options(CLIOptions, HasCLIOptions(..))

import qualified IFSConfig

import qualified Interactive

import Control.Monad
import qualified System.Exit
import qualified Data.Text.IO
import qualified System.Random
import Lens.Micro.Platform

main :: IO ()
main = do
  options <- Options.parseCommandLineOptions

  when ((options^.samples) < (options^.paralellism))
    (System.Exit.die "Error. `samples` should be larger than `paralellism`.")

  options' <- maybeSeedRNG options
  runProgram options'

maybeSeedRNG :: CLIOptions -> IO CLIOptions
maybeSeedRNG options = do
  seed_val <- maybeSeedRNG' options
  putStrLn ("Using seed " ++ (show seed_val))
  return (set seed (Just seed_val) options)
  where
    maybeSeedRNG' options =
      case options^.seed of
        Nothing ->
          System.Random.getStdRandom System.Random.random
        Just val ->
          return val

runProgram :: CLIOptions -> IO ()
runProgram options = do
  stdin <- Data.Text.IO.getContents
  ifsdata <- IFSConfig.read stdin

  Interactive.run ifsdata options
