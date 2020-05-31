module Lib.MortonCode (
  expandBits,
  interleaveBits,
  shrinkBits,
  deinterleaveBits,
    ) where

import qualified Prelude as P
import qualified Data.List as DL
import qualified Helper as Helper
import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits


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