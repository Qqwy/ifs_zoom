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
  binarySearch,
  chaosGame,
  xorShift,
  wordToDouble,
  randomVector,
  radixSort

) where

import qualified Prelude as P
import Pipe
import Data.Array.Accelerate
import Data.Bits as Bits
import Data.Array.Accelerate.Data.Bits as ABits

-- | A simple vector inner product
--
-- (came with the Accelerate project scaffold. Left in the project for now
-- to easily test a simple parallel function that works.)
dotp :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp xs ys = fold (*) 1 ( zipWith (+) xs ys)

-- | An implementation of Binary Search
-- Returns the index at which `target` appears in `arr`.
-- Assumes that `arr` is sorted.
--
-- When `target` is not in `arr` we return the index at which it would appear in the sorted array.
--
-- This implementation by itself is not parallel,
-- but it can be used as (sequential) building block in a larger (parallel) algorithm.
--
-- ## Implementation details:
--
-- Note the `the` viewpattern to extract the incoming target from a singleton (CPU -> GPU) array
binarySearch :: Ord a => Acc (Scalar a) -> Acc (Vector a) -> Acc (Scalar Int)
binarySearch (the -> target) arr =
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


-- | Runs the chaos game in parallel
--
-- Taking a number of points `nThreads * nPointsPerThread` and the set of `transformations` (with probabilities) as input
-- and returns a sequence with `n` points as output
--
-- - Build a matrix of nThreads * nPointsPerThread
-- - In parallel for each row
--   - Compute the next point by applying a randomly chosen transformation to the current point (scanl).
-- - Reinterpret the resulting matrix as a single long sequence
chaosGame :: Int -> Int -> Acc (Vector Double) -> Acc (Vector (Double, Double))
chaosGame nThreads nPointsPerThread transformations =
  mat
  |> scanl transformPoint (lift (0.0, 0.0))
  |> reshape (constant (Z :. nPointsPerThread * nThreads))
  where
    mat :: Acc (Matrix (Double, Double))
    mat = fill (constant (Z :. nPointsPerThread :. nThreads)) (lift (0, 0))
    -- startingPoints = fromList (Z :. nThreads) [(0, 0)..] -- random points between (-1, -1) and (1, 1)
    transformPoint :: Exp (Double, Double) -> Exp (Double, Double) -> Exp (Double, Double)
    transformPoint point acc = point
      -- TODO Pick one of the transformations and apply it to the point
      -- TODO randomness. Maybe xorshift?

-- | Simple XorShift Random Number Generator
-- Returns a new number based on the given one.
-- Note that XorShift is an _ok_ RNG but not a very high-quality one:
-- Specifically, it will fail certain statistical tests.
-- Also, it will never return `0` (if given any number except `0`).
xorShift :: ABits.Bits a => Exp a -> Exp a
xorShift a =
  let
    b = a `ABits.xor` (ABits.shiftL a 13)
    c = b `ABits.xor` (ABits.shiftR b  7)
    d = c `ABits.xor` (ABits.shiftL c 17)
  in
    d

wordToDouble :: Exp Word -> Exp Double
wordToDouble input =
  (fromIntegral input) / (fromIntegral highest)
  where
    highest = maxBound :: Exp Word64


randomVector :: Int -> Exp Word -> Acc (Vector Word)
randomVector length seed =
  emptyVector
  |> scanl (\ seed _-> xorShift seed) seed
  where
    emptyVector = fill (constant (Z :. length)) 0

-- | A full Radix Sort
--
-- ## Implementation notes:
--
-- This implementation uses 64 unrolled implementations of `radixSortBit`,
-- one for each bit in a 64-bit number.
--
-- The nice thing about using Accelerate
-- is that we can manipulate the AST creation using techniques like this one
-- where we can create an unrolled version of something that would be horrible to write by hand.
radixSort :: Acc (Vector Word) -> Acc (Vector Word)
radixSort vector =
  bits
  |> P.map Lib.radixSortBit
  |> P.foldl (|>) vector
  where
    bits = [0..63]


-- | One step of parallel radix sort, for a single bit.
radixSortBit :: Int -> Acc (Vector Word) -> Acc (Vector Word)
radixSortBit bit vector =
  vector
  |> scatter sorted_indexes vector
  where
    ones = map (\elem -> if ABits.testBit elem bit then 1 else 0) vector
    zeroes = map (\elem -> 1 - elem) ones
    (ones_sum, n_ones) = (afst res, res |> asnd |> the)
      where
        res = scanl' (+) 0 ones
    (zeroes_sum, n_zeroes) = (afst res, res |> asnd |> the)
      where
        res = scanl' (+) 0 zeroes
    sorted_indexes =
      zip3 ones zeroes_sum ones_sum
      |> map (\(unlift -> (isone, ifzero_idx, ifone_idx)) ->
                if isone == 1
                then n_zeroes + ifone_idx
                else ifzero_idx
             )
