{-# LANGUAGE ViewPatterns #-}

{-|
 Module      : Lib.Random
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

Generate random data in parallel on the GPU, based on a single seed.

We use the `xorShift` method to generate data. This is not a very high-quality generator,
but it should be good enough for our purposes.
 -}


module Lib.Random
  (
    randomVector,
    randomMatrix,
    xorShift64,
    extractRandomUnitIntervalDouble
  ) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Lib.MortonCode

-- | Fills a vector of `length` length with random numbers using `seed` as starting seed.
randomVector :: Int -> Word64 -> Acc (Vector Word64)
randomVector length seed =
  emptyVector
  |> scanl (\ prev _-> xorShift64Star prev) seed'
  where
    seed' = xorShift64Star (constant seed)
    emptyVector = fill (constant (Z :. length)) 0

-- | A cute implementation of a 'random' matrix.
--
-- However, since we do not fully split the RNG,
-- some entries to the top-left of the matrix might be close to each-other.
--
-- If better RNG is needed, we might be able to use something like the `jump`
-- mentioned on http://prng.di.unimi.it/
randomMatrix :: Int -> Int -> Word64 -> Acc (Matrix Word64)
randomMatrix n_cols n_rows seed =
    nums
    |> fillFirstColumn
    |> fillRows
    where
      nums =
        seed
        |> randomVector n_rows
        |> imap (\(unindex1 -> index) content -> content + (fromIntegral index))
      zeroes = fill (constant (Z :. n_rows :. n_cols)) 0
      mapIndexToCol0 :: Exp DIM1 -> Exp DIM2
      mapIndexToCol0 (unindex1 -> index) = index2 index 0
      fillFirstColumn nums = permute const zeroes mapIndexToCol0 nums
      fillRows mat = scanl1 (\acc _-> xorShift64Star acc) mat

-- | Simple XorShift Random Number Generator
-- Returns a new number based on the given one.
-- Note that XorShift is an _ok_ RNG but not a very high-quality one:
-- Specifically, it will fail certain statistical tests.
-- Also, it will never return `0` (if given any number except `0`).
xorShift64 :: Exp Word64 -> Exp Word64
xorShift64 a =
  let
    b = a `xor` (shiftL a 13)
    c = b `xor` (shiftR b  7)
    d = c `xor` (shiftL c 17)
  in
    d

-- | Simple XorShift Random Number Generator
--
-- This one is slightly better than the normal `xorShift64`
xorShift64Star :: Exp Word64 -> Exp Word64
xorShift64Star a =
  let
    b = a `xor` (shiftR a 12)
    c = b `xor` (shiftL b 25)
    d = c `xor` (shiftR c 27)
  in
    d * 2685821657736338717

-- | Returns a double in the half-open range [0, 1)
-- based on the random unsigned integer that was generated.
--
-- This uses the notes at http://prng.di.unimi.it/. Specifically:
-- A double has a significant binary digits 53.
-- We extract thus the top 53 bits of the number (which are better quality than the lower 11)
-- We multiply this by `0x1.0p-53` (that is: 1.0 * 2^-53) to obtain a number in the half-open interval [0, 1)
extractRandomUnitIntervalDouble :: Exp Word64 -> Exp Double
extractRandomUnitIntervalDouble word =
  word
  |> (`shiftR` 11)
  |> fromIntegral
  |> (* (recip 2**53))
