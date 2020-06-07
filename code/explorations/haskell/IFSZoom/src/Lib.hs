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
  binarySearch,
  randomMatrix,
  -- randomVector,
  -- xorShift,
  -- radixSort,
  -- radixSortBit,
  -- wordToNormalizedDouble,
  -- expandBits,
  -- interleaveBits,
  -- shrinkBits,
  -- deinterleaveBits,
  sortPoints
) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Lib.MortonCode
import Lib.Random
import Lib.Sort


-- |
-- Main function to run once on the GPU
-- when the simulation starts
-- to create a representation of a point cloud
-- that we can quickly search through to build individual frames.
--
-- Note that this implementation is unfinished (c.f. lots of `undefined`s)
-- but it gives a general overview of how far the work has proceeded.
createIFSPointCloud nThreads nPoints transformations =
  transformations
  |> chaosGame nThreads nPoints
  |> buildBinarySearchTree
  where
    chaosGame = undefined

--- Below are ideas that are only useful later:

-- | Transforms an array of points into a binary search tree to quickly search through
--
-- Note that this implementation is unfinished (c.f. lots of `undefined`s)
-- but it gives a general overview of how far the work has proceeded.
buildBinarySearchTree points =
  points
  |> sortPoints
  |> buildInplaceBinarySearchTree
  where
    buildInplaceBinarySearchTree = undefined


-- | An implementation of Binary Search
-- Returns the index at which `target` appears in `arr`.
-- Assumes that `arr` is sorted.
--
-- When `target` is not in `arr` we return the index at which it would appear in the sorted array.
--
-- This implementation by itself is not parallel,
-- but it can be used as (sequential) building block in a larger (parallel) algorithm.
--
binarySearch :: Ord a => Exp a -> Acc (Vector a) -> Acc (Scalar Int)
binarySearch target arr =
  initial_bounds
  |> while targetNotFound shrinkBounds
  |> fst  -- (middle, middle) -> middle
  |> unit -- wrap as single-element array, because Accelerate always has to return an array from the GPU -> CPU
      where
        initial_bounds :: Exp (Int, Int)
        initial_bounds = lift (0, length arr - 1)

        targetNotFound :: Exp (Int, Int) -> Exp Bool
        targetNotFound (unlift -> (left, right)) = (arr !! left) < (arr !! right)

        shrinkBounds :: Exp (Int, Int) -> Exp (Int, Int)
        shrinkBounds (unlift -> (left, right)) =
          let
            middle = (left + right) `quot` 2
            elem = arr !! middle
          in
            if      elem < target then lift $ (middle + 1, right)
            else if elem > target then lift $ (left, middle - 1)
            else                       lift $ (middle, middle)
