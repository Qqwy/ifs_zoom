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

import Lib

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

main :: IO ()
main = do

  -- runExample
  runBinarySearch

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
runBinarySearch = do
  let
    arr :: Acc (Vector Int)
    arr = use $ fromList (Z:.10) [1,3..]
    elem = unit 122 :: Acc (Scalar Int)
    acc_ast = binarySearch

  printf "the function to execute:\n"
  printf "%s\n\n" (show acc_ast)

  -- printf "the input to execute:\n"
  -- printf "%s\n\n" (show input)

  printf "result with interpreter backend: %s\n" (show (Interpreter.runN $ acc_ast elem arr))
  let
    res = (Interpreter.runN $ acc_ast elem arr)
  printf "result: %s\n" (show (res S.! Z))
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
  printf "result with CPU backend: %s\n" (show (CPU.runN $ acc_ast elem arr))
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
  printf "result with PTX backend: %s\n" (show (PTX.runN $ acc_ast elem arr))
#endif
