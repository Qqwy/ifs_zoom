{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RebindableSyntax #-}

module Lib.Picture
  (
    naivePointCloudToPicture
  ) where

import Pipe
import Lib.Common
import qualified Lib.Camera
import Lib.Camera (Camera)

import Data.Array.Accelerate as Accelerate
import Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Data.Colour.RGB as RGB
import Data.Array.Accelerate.Data.Colour.Names

type RasterPicture = Matrix Word32


naivePointCloudToPicture :: Exp Camera -> Exp Int -> Exp Int -> Acc (Vector Point) -> Acc RasterPicture
naivePointCloudToPicture camera width height point_cloud =
  point_cloud
  |> worldToScreen camera
  |> screenToPixels width height
  |> pixelsToColours

worldToScreen :: Exp Camera -> Acc (Vector Point) -> Acc (Vector Point)
worldToScreen camera point_cloud =
  point_cloud
  |> Accelerate.map (Lib.Common.pointToHomogeneous)
  |> Accelerate.map (Lib.Camera.cameraTransform camera)
  |> Accelerate.map (Lib.Common.homogeneousToPoint)

screenToPixels :: Exp Int -> Exp Int -> Acc (Vector (Float, Float)) -> Acc (Matrix Int)
screenToPixels width height input = permute (+) zeros (mapping input) (ones input)
  where
    zeros :: Acc (Matrix Int)
    zeros = fill (index2 height width) 0
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

-- | Turns a point to a pixel coordinate using the given `width` and `height` as picture dimensions.
--
-- This uses the special `ignore` index for points/pixels that fall outside of the region of the picture.
--
-- TODO this implementation is currently hard-coded for the Barnsley Fern,
-- we want to use a customizable camera transformation instead.
pointToPixel :: Exp Int -> Exp Int -> Exp (Float, Float) -> Exp DIM2
pointToPixel width height (unlift -> (x, y)) =
  cond
  (pointVisible width height (lift (xpos, ypos)))
  (index2 ypos xpos)
  (ignore)
  where
    x' = x :: Exp Float
    y' = y :: Exp Float
    xpos = x' * (fromIntegral width)  |> Accelerate.floor
    ypos = y' * (fromIntegral height) |> Accelerate.floor
    -- xpos =
    --   (fromIntegral width / 2) + x' * (fromIntegral width) / 11
    --   |> Accelerate.floor
    -- ypos =
    --   ((fromIntegral height)) - y' * (fromIntegral height) / 11
    --   |> Accelerate.floor

pointVisible :: Exp Int -> Exp Int -> Exp (Int, Int) -> Exp Bool
pointVisible width height (unlift -> (x, y)) =
  x >= 0 && x < width &&
  y >= 0 && y < height
