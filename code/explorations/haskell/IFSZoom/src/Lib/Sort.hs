{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-|
 Module      : Lib.Sort
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

This module deals with the sorting of data in parallel on the GPU.

The main entrypoint is `sortPoints`.
The other exposed functions are building blocks of that one.
They are exposed to be easily testable themselves.
 -}

module Lib.Sort
  (
    sortPoints
  , radixSort
  , radixSortBit
  ) where

import Pipe

import qualified Prelude
import qualified Data.List
import qualified Helper as Helper
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits

import qualified Lib.MortonCode

-- | Sorts an array of points by the Z-order curve (AKA morton code)
--
-- This is done by transforming the pair of 32-bit floats to 32-bit unsigned int,
-- and combining them to get one unsigned 64-bit int.
-- After sorting, this transformation is reversed.
sortPoints :: Acc (Vector (Float, Float)) -> Acc (Vector (Float, Float))
sortPoints points =
  points
  |> map floatPairToWord32Pair
  |> map (uncurry Lib.MortonCode.interleaveBits)
  |> radixSort
  |> map Lib.MortonCode.deinterleaveBits
  |> map word32ToFloatPair
  where
    floatPairToWord32Pair :: Exp (Float, Float) -> Exp (Word32, Word32)
    floatPairToWord32Pair (unlift -> (x, y)) = lift (bitcast x, bitcast y)
    word32ToFloatPair :: Exp (Word32, Word32) -> Exp (Float, Float)
    word32ToFloatPair (unlift -> (x, y)) = lift (bitcast x, bitcast y)



-- | A full parallel Radix Sort
--
-- ## Implementation notes:
--
-- This implementation uses 64 unrolled implementations of `radixSortBit`,
-- one for each bit in a 64-bit number.
--
-- The nice thing about using Accelerate
-- is that we can manipulate the AST creation using techniques like this one
-- where we can create an unrolled version of something that would be horrible to write by hand.
--
-- ## Examples
--
-- >>> import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- >>> arr = use $ fromList (Z :. 10) ([10, 9..]) :: Acc (Vector Word64)
-- >>> CPU.run $ Lib.Sort.radixSort arr
-- Vector (Z :. 10) [1,2,3,4,5,6,7,8,9,10]
radixSort :: Acc (Vector Word64) -> Acc (Vector Word64)
radixSort vector =
  Helper.bitsList
  |> Prelude.fmap radixSortBit
  |> Data.List.foldl' (\input next -> input |> compute |>next) vector

-- | One step of parallel radix sort, for a single bit.
radixSortBit :: Int -> Acc (Vector Word64) -> Acc (Vector Word64)
radixSortBit sort_bit vector =
  vector
  |> scatter sorted_indexes vector
  where
    ones = map (\elem -> if testBit elem (constant sort_bit) then 1 else 0) vector
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
