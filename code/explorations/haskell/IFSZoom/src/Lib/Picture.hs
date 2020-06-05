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



naivePointCloudToPicture :: Acc (Vector (Float, Float)) -> Acc (Matrix Word32)
naivePointCloudToPicture point_cloud =
  point_cloud
  |> cloudToPixels
  |> pixelsToColours

cloudToPixels :: Acc (Vector (Float, Float)) -> Acc (Matrix Int)
cloudToPixels input = permute (+) zeros (mapping input) (ones input)
  where
    zeros :: Acc (Matrix Int)
    zeros = fill (constant (Z :. height :. width)) 0
    ones :: Acc (Vector (Float, Float)) -> Acc (Vector Int)
    ones input = fill (shape input) 1
    mapping :: Acc (Vector (Float, Float)) -> Exp DIM1 -> Exp DIM2
    mapping input index = pointToPixel width height (input ! index)
    width = 400 :: Int
    height = 400 :: Int


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
  index2 xpos ypos
  where
    xpos =
      (x :: Exp Float) * (constant (Prelude.fromIntegral width))
      |> Accelerate.floor
      -- x
      -- |> Accelerate.round
      -- |> (flip mod) (constant width)
    ypos =
      (y :: Exp Float) * (constant (Prelude.fromIntegral height))
      |> Accelerate.floor
      -- y
      -- |> Accelerate.round
      -- |> (flip mod) (constant height)

