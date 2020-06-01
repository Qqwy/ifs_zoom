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
    xorShift
  ) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits
import Lib.MortonCode

-- | Fills a vector of `length` length with random numbers using `seed` as starting seed.
randomVector :: Int -> Word -> Acc (Vector Word)
randomVector length seed =
  emptyVector
  |> scanl (\ prev _-> xorShift prev) seed'
  where
    seed' = xorShift (constant seed)
    emptyVector = fill (constant (Z :. length)) 0

-- | A cute implementation of a 'random' matrix.
--
-- However, since we do not fully split the RNG,
-- some entries to the top-left of the matrix might be close to each-other.
--
-- If better RNG is needed, we might be able to use something like the `jump`
-- mentioned on http://prng.di.unimi.it/
randomMatrix :: Int -> Int -> Word -> Acc (Matrix Word)
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
      fillRows mat = scanl1 (\acc _-> xorShift acc) mat

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
