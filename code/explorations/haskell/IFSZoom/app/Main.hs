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
import Lib
import qualified Lib.ChaosGame
import qualified Lib.Sort
import qualified Lib.Picture

import Text.Printf
import Prelude                                                      as P

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.Interpreter                            as Interpreter
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import Data.Array.Accelerate.LLVM.Native                            as CPU
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import Data.Array.Accelerate.LLVM.PTX                               as PTX
#endif

import Data.Array.Accelerate.Array.Sugar                        as S

import Graphics.Gloss
import qualified Data.Array.Accelerate.IO.Codec.BMP as IOBMP


import qualified Debug.Trace

main :: IO ()
main = do
  --display (InWindow "Nice Window" (800, 600) (10, 10)) black (Color white $ Circle 80)
  -- let
  --   mycircle = Circle 80 |> Color white
  --   dimensions = (800, 600)
  --   position = (10, 10)
  --   window = (InWindow "Iterated Function Systems Exploration" dimensions position)

  -- (mycircle |> display window black)

  runChaosGame


runChaosGame :: IO ()
runChaosGame = do
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
    seed = 42
    sqrt_npoints = 12000
    -- arr :: S.Vector (Float, Float)
    -- arr = fromList (Z :. 100) [(x, y) | x <- [0..10], y <- [0..10]]
    program1 =
      seed
      |> Lib.ChaosGame.chaosGame transformations sqrt_npoints
      -- |> Debug.Trace.traceShowId
      -- |> Lib.Sort.sortPoints
      -- use arr
    result = PTX.run program1
    program2 =
      -- (use result)
      program1
      |> Lib.Picture.naivePointCloudToPicture
    result2 =
      PTX.run program2
      |> IOBMP.writeImageToBMP "example_picture.bmp"

  -- printf "program1: %s\n" (show program1)
  -- printf "program2: %s\n" (show program2)
  -- printf "result: %s\n" (result |> A.toList |> show)
  -- printf "output (first 100 elements): %s\n" (result |> A.toList |> show)

  result2
  
  -- runExample
  -- runBinarySearch
  -- runSort

-- runExample = do
--   let
--       xs :: Vector Int
--       xs = fromList (Z:.10) [0..]

--       ys :: Vector Int
--       ys = fromList (Z:.10) [1,3..]

--   printf "input data:\n"
--   printf "xs = %s\n" (show xs)
--   printf "ys = %s\n\n" (show ys)

--   printf "the function to execute:\n"
--   printf "%s\n\n" (show dotp)

--   printf "result with interpreter backend: dotp xs ys = %s\n" (show (Interpreter.runN dotp xs ys))
-- #ifdef ACCELERATE_LLVM_NATIVE_BACKEND
--   printf "result with CPU backend: dotp xs ys = %s\n" (show (CPU.runN dotp xs ys))
-- #endif
-- #ifdef ACCELERATE_LLVM_PTX_BACKEND
--   printf "result with PTX backend: dotp xs ys = %s\n" (show (PTX.runN dotp xs ys))
-- #endif

-- runBinarySearch :: (Acc (Vector Int) -> Exp Int -> Acc (Scalar Int)) -> Acc (Vector Int) -> Exp Int -> IO ()
-- runBinarySearch = do
--   let
--     arr :: Acc (Vector Int)
--     arr = use $ fromList (Z:.10) [1,3..]
--     elem = unit 122 :: Acc (Scalar Int)
--     acc_ast = binarySearch

--   printf "the function to execute:\n"
--   printf "%s\n\n" (show acc_ast)

--   -- printf "the input to execute:\n"
--   -- printf "%s\n\n" (show input)

--   printf "result with interpreter backend: %s\n" (show (Interpreter.runN $ acc_ast elem arr))
--   let
--     res = (Interpreter.runN $ acc_ast elem arr)
--   printf "result: %s\n" (show (res S.! Z))
-- #ifdef ACCELERATE_LLVM_NATIVE_BACKEND
--   printf "result with CPU backend: %s\n" (show (CPU.runN $ acc_ast elem arr))
-- #endif
-- #ifdef ACCELERATE_LLVM_PTX_BACKEND
--   printf "result with PTX backend: %s\n" (show (PTX.runN $ acc_ast elem arr))
-- #endif

-- runSort = do
--   let
    -- arr :: Acc (Vector (Float, Float))
    -- -- arr = use $ fromList (Z :. 100) ([100, 99..])
    -- arr = use $ fromList (Z :. 10000) ([(x,y) | x <- [100,99..1], y <- [100,99..1]])
    -- program = arr |> A.map (A.uncurry Lib.interleaveBits) |> Lib.radixSort |> A.map Lib.deinterleaveBits
    -- program = arr |> Lib.sortPoints
  --   program = randomMatrix 10 10 20

  -- -- printf "as input: %s\n" (show arr)
  -- -- printf "result with interpreter backend: %s\n" (show (Interpreter.runN $ Lib.radixSort arr))
  -- printf "result with CPU backend: %s\n" (show (CPU.runN $ program))
  -- printf "result with PTX backend: %s\n" (show (PTX.runN $ program))
