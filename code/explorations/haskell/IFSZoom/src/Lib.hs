{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Lib
-- Copyright   : [2020] Wiebe-Marten Wijnja
-- License     : BSD3
--
-- Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Lib (

  dotp,
  binarySearch

) where

import Data.Array.Accelerate

-- | A simple vector inner product
--
dotp :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp xs ys = fold (*) 1 ( zipWith (+) xs ys)


binarySearch :: Ord a => Acc (Vector a) -> Acc (Scalar a) -> Acc (Scalar Int)
binarySearch arr target =
  let
    target' = the target
  in
    unit $ fst $ binarySearch' arr target'

binarySearch' :: Ord a => Acc (Vector a) -> Exp a -> Exp (Int, Int)
binarySearch' arr target =
    while check body initial
  where
    initial :: Exp (Int, Int)
    initial = lift (0, length arr - 1)

    check :: Exp (Int, Int) -> Exp Bool
    check (unlift -> (left, right)) = (arr !! left) < (arr !! right)

    body :: Exp (Int, Int) -> Exp (Int, Int)
    body (unlift -> (left, right)) =
      let
        middle = (left + right) `quot` 2
        elem = arr !! middle
      in
        if      elem < target then lift $ (middle + 1, right)
        else if elem > target then lift $ (left, middle - 1)
        else                       lift $ (middle, middle)
