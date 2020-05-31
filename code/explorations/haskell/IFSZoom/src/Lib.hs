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
  chaosGame,
  xorShift,
  wordToDouble,
  randomVector,
  radixSort,
  radixSortBit,
  expandBits,
  interleaveBits,
  shrinkBits,
  deinterleaveBits,
  sortPoints

) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits


-- |
-- Main function to run once on the GPU
-- when the simulation starts
-- to create a representation of a point cloud
-- that we can quickly search through to build individual frames.
--
-- Note that this implementation is unfinished (c.f. lots of `undefined`s)
-- but it gives a general overview of how far the work has proceeded.
createIFSPointCloud nThreads nPoints transformations =
  chaosGame nThreads nPoints transformations
  |> buildBinarySearchTree
  where
    chaosGame = undefined

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

-- | Sorts an array of points by the Z-order curve (AKA morton code)
--
-- This is done by transforming the pair of 32-bit floats to 32-bit unsigned int,
-- and combining them to get one unsigned 64-bit int.
-- After sorting, this transformation is reversed.
sortPoints :: Acc (Vector (Float, Float)) -> Acc (Vector (Float, Float))
sortPoints points =
  points
  |> map floatPairToWord32Pair
  |> map (uncurry interleaveBits)
  |> radixSort
  |> map deinterleaveBits
  |> map word32ToFloatPair
  where
    floatPairToWord32Pair :: Exp (Float, Float) -> Exp (Word32, Word32)
    floatPairToWord32Pair (unlift -> (x, y)) = lift (bitcast x, bitcast y)
    word32ToFloatPair :: Exp (Word32, Word32) -> Exp (Float, Float)
    word32ToFloatPair (unlift -> (x, y)) = lift (bitcast x, bitcast y)


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
xorShift :: Bits a => Exp a -> Exp a
xorShift a =
  let
    b = a `xor` (shiftL a 13)
    c = b `xor` (shiftR b  7)
    d = c `xor` (shiftL c 17)
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
radixSort :: Acc (Vector Word64) -> Acc (Vector Word64)
radixSort vector =
  Helper.bitsList
  |> P.map Lib.radixSortBit
  |> DL.foldl' (\input next -> input |> compute |>next) vector

-- | One step of parallel radix sort, for a single bit.
radixSortBit :: Int -> Acc (Vector Word64) -> Acc (Vector Word64)
radixSortBit bit vector =
  vector
  |> scatter sorted_indexes vector
  where
    ones = map (\elem -> if testBit elem (constant bit) then 1 else 0) vector
    zeroes = map (\elem -> 1 - elem) ones
    (zeroes_sum, n_zeroes) = let res = scanl' (+) 0 zeroes in (afst res, the (asnd res))
    ones_sum = scanl (+) 0 ones
    sorted_indexes =
      zip3 ones zeroes_sum ones_sum
      |> map (\(unlift -> (isone, ifzero_index, ifone_index)) ->
                if isone == 1
                then n_zeroes + ifone_index
                else ifzero_index
             )

-- | Returns a single 64-bit number where all even-indexed bits are set to the bits of `a`.
--
-- So the MSB is 0, the next bit is the MSB of `a`, the next is 0, the next is next-highest bit of `a` etc.
--
-- Kudos to https://lemire.me/blog/2018/01/08/how-fast-can-you-bit-interleave-32-bit-integers/
-- for explaining this bit-twiddling technique in detail.
expandBits ::  Exp Word32 ->  Exp Word64
expandBits input =
  input
  |> unlift
  |> (fromIntegral :: Exp Word32 -> Exp Word64)
  |> expand
  where
    expand :: Exp Word64 -> Exp Word64
    expand num = DL.foldl' expandStep num constants
      where
        expandStep a (offset, mask) = (a `xor` (a `shiftL` offset)) .&. mask
        constants =
          [
            (16, 0x0000ffff0000ffff),
            (8, 0x00FF00FF00FF00FF),
            (4, 0x0F0F0F0F0F0F0F0F),
            (2, 0x3333333333333333),
            (1, 0x5555555555555555)
          ]

-- | Returns the morton-code interleaving
--
-- where from MSB to LSB we have one bit of y and then of x
-- (and then the next-highest bit of y and then the next-highest bit of x etc).
interleaveBits ::  Exp Word32 ->  Exp Word32 ->  Exp Word64
interleaveBits x y =
  let
    x' = expandBits x
    y' = expandBits y
  in
    x' .|. (y' `shiftL` 1)

shrinkBits ::  Exp Word64 ->  Exp Word32
shrinkBits input =
  input
  |> maskOddBits
  |> shrink
  |> (fromIntegral :: Exp Word64 -> Exp Word32)
  where
    maskOddBits :: Exp Word64 -> Exp Word64
    maskOddBits num = num .&. 0x5555555555555555
    shrink :: Exp Word64 -> Exp Word64
    shrink num = DL.foldl' shrinkStep num constants
      where
        shrinkStep a (offset, mask) = (a `xor` (a `shiftR` offset)) .&. mask
        constants =
          [
            (1, 0x3333333333333333),
            (2, 0x0F0F0F0F0F0F0F0F),
            (4, 0x00FF00FF00FF00FF),
            (8, 0x0000ffff0000ffff),
            (16, 0x00000000ffffffff)
          ]

deinterleaveBits ::  Exp Word64 -> Exp (Word32,  Word32)
deinterleaveBits res =
  let
    x = shrinkBits res
    y = shrinkBits (res `shiftR` 1)
  in
    lift (x, y)
