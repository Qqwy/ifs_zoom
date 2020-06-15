{-# LANGUAGE TemplateHaskell #-}

module Options
  ( parseCommandLineOptions
  , commandLineOptionsWithHelp 
  , commandLineOptions
  , CLIOptions(..)
  , HasCLIOptions(..)
  ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Word

import Lens.Micro.Platform

data CLIOptions = CLIOptions
  { _samples :: Word64
  , _paralellism :: Word64
  , _seed :: Word64
  , _render_width :: Word
  , _render_height :: Word
  }
  deriving (Show)

makeClassy ''CLIOptions

parseCommandLineOptions :: IO CLIOptions
parseCommandLineOptions = execParser commandLineOptionsWithHelp

commandLineOptionsWithHelp :: ParserInfo CLIOptions
commandLineOptionsWithHelp = info (commandLineOptions <**> helper)
  (
    fullDesc
    <> progDesc "Generates an Iterated Function System as a picture."
    <> header "IFSZoom - Zooming into Iterated Function Systems"
  )

commandLineOptions :: Parser CLIOptions
commandLineOptions = CLIOptions
  <$> option auto
  ( long "samples"
    <> short 's'
    <> metavar "N_SAMPLES"
    <> help "The number of samples or points to generate using the 'chaos game' rendering method."
    <> showDefault
    <> value 10
  )

  <*> option auto
  ( long "paralellism"
    <> short 'p'
    <> metavar "PARALELLISM"
    <> help "The number of GPU-threads to split the number of samples across. More threads means faster generation, but generation will be less 'deep'. Usually you'd want as many threads as your GPU has cores. `samples` has to be larger than `paralellism`."
    <> showDefault
    <> value 2048
  )

  <*> option auto
  ( long "seed"
    <> metavar "SEED"
    <> help "The random number seed to use. When not supplied, will seed with an arbitrary value instead."
    <> value 0
  )

  <*> option auto
  (
    long "render_width"
    <> short 'w'
    <> metavar "WIDTH"
    <> help "The width in pixels of the rendering we are generating."
    <> showDefault
    <> value 800
  )

  <*> option auto
  (
    long "render_height"
    <> short 'w'
    <> metavar "HEIGHT"
    <> help "The height in pixels of the rendering we are generating."
    <> showDefault
    <> value 800
  )
