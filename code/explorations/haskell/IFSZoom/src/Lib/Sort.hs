{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module Lib.Sort
  (
    radixSort,
    radixSortBit
  ) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits

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
  |> P.map radixSortBit
  |> DL.foldl' (\input next -> input |> compute |>next) vector

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
