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

import Pipe
import Data.Array.Accelerate

-- | A simple vector inner product
--
dotp :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp xs ys = fold (*) 1 ( zipWith (+) xs ys)


binarySearch :: Ord a => Acc (Vector a) -> Acc (Scalar a) -> Acc (Scalar Int)
binarySearch arr (the -> target) =
  initial_bounds
  |> while targetNotFound halveBounds
  |> fst
  |> unit
      where
        initial_bounds :: Exp (Int, Int)
        initial_bounds = lift (0, length arr - 1)

        targetNotFound :: Exp (Int, Int) -> Exp Bool
        targetNotFound (unlift -> (left, right)) = (arr !! left) < (arr !! right)

        halveBounds :: Exp (Int, Int) -> Exp (Int, Int)
        halveBounds (unlift -> (left, right)) =
          let
            middle = (left + right) `quot` 2
            elem = arr !! middle
          in
            if      elem < target then lift $ (middle + 1, right)
            else if elem > target then lift $ (left, middle - 1)
            else                       lift $ (middle, middle)
