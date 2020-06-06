{-# LANGUAGE ViewPatterns #-}

module Lib.Picture
  (
    naivePointCloudToPicture
  ) where

import Pipe
import Data.Array.Accelerate as Accelerate
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.Data.Colour.Names



naivePointCloudToPicture :: Int -> Int -> Acc (Vector (Float, Float)) -> Acc (Matrix Word32)
naivePointCloudToPicture width height point_cloud =
  point_cloud
  |> cloudToPixels width height
  |> pixelsToColours

cloudToPixels :: Int -> Int -> Acc (Vector (Float, Float)) -> Acc (Matrix Int)
cloudToPixels width height input = permute (+) zeros (mapping input) (ones input)
  where
    zeros :: Acc (Matrix Int)
    zeros = fill (constant (Z :. width :. height)) 0
    ones :: Acc (Vector (Float, Float)) -> Acc (Vector Int)
    ones array = fill (shape array) 1
    mapping :: Acc (Vector (Float, Float)) -> Exp DIM1 -> Exp DIM2
    mapping array index = pointToPixel width height (array ! index)


pixelsToColours :: Acc (Matrix Int) -> Acc (Matrix Word32)
pixelsToColours pixels =
  pixels
  |> Accelerate.map pixelToColour
  |> Accelerate.map HSL.toRGB
  |> Accelerate.map RGB.packRGB
  where
    pixelToColour :: Exp Int -> Exp (HSL Float)
    pixelToColour pixel = Accelerate.cond (pixel Accelerate.== 0) black white


pointToPixel :: Int -> Int -> Exp (Float, Float) -> Exp DIM2
pointToPixel width height (unlift -> (x, y)) =
  index2 ypos xpos
  where
    x' = x :: Exp Float
    y' = y :: Exp Float
    xpos =
      (constant (Prelude.fromIntegral width / 2)) + x' * (Prelude.fromIntegral width) / 11
      |> Accelerate.floor
      -- (x :: Exp Float) * (constant (Prelude.fromIntegral width))
      -- |> Accelerate.floor
      -- x
      -- |> Accelerate.round
      -- |> (flip mod) (constant width)
    ypos =
      (constant (Prelude.fromIntegral height)) - y' * (Prelude.fromIntegral height) / 11
      |> Accelerate.floor
      -- (y :: Exp Float) * (constant (Prelude.fromIntegral height))
      -- |> Accelerate.floor
      -- -- y
      -- |> Accelerate.round
      -- |> (flip mod) (constant height)

