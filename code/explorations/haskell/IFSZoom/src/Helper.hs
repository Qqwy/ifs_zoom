module Helper (
  bitsList,
  interleaveBits,
  expandBits
  ) where

import Data.Bits
import Data.Word

bitsList :: [Int]
bitsList = [0..63]


-- | Returns a single 64-bit number where all even-indexed bits are set to the bits of `a`.
--
-- So the MSB is 0, the next bit is the MSB of `a`, the next is 0, the next is next-highest bit of `a` etc.
expandBits :: Word32 -> Word64
expandBits a =
  let
    constants =
      [
        (16, 0x0000ffff0000ffff),
        (8, 0x00FF00FF00FF00FF),
        (4, 0x0F0F0F0F0F0F0F0F),
        (2, 0x3333333333333333),
        (1, 0x5555555555555555)
      ]
    step num (offset, mask) = (num `xor` (num `shiftL` offset)) .&. mask
  in
    foldl step (fromIntegral a) constants

-- | Returns the morton-code interleaving
--
-- where from MSB to LSB we have one bit of y and then of x
-- (and then the next-highest bit of y and then the next-highest bit of x etc).
interleaveBits :: Word32 -> Word32 -> Word64
interleaveBits x y =
  let
    x' = expandBits x
    y' = expandBits y
  in
    x' .|. (y' `shiftL` 1)
