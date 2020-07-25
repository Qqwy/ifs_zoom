{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RebindableSyntax #-}

{-|
 Module      : Lib.Picture
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

Transforming a point cloud to a rasterized picture, using a camera transformation.
-}
module Lib.Picture
  ( RasterPicture
  , naivePointCloudToPicture
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

-- | A very simple way to create a picture from a point cloud.
-- No optimizations: we iterate over all points
-- and only find out very late whether they'll end up on the screen.
naivePointCloudToPicture :: Exp Camera -> Exp Int -> Exp Int -> Acc (Vector Point) -> Acc RasterPicture
naivePointCloudToPicture camera width height point_cloud =
  point_cloud
  |> worldToScreen camera
  |> screenToPixels width height
  |> pixelsToColours

-- | Maps the camera transformation over all points.
worldToScreen :: Exp Camera -> Acc (Vector Point) -> Acc (Vector Point)
worldToScreen camera point_cloud =
  point_cloud
  |> Accelerate.map (Lib.Common.pointToHomogeneous)
  |> Accelerate.map (Lib.Camera.cameraTransform camera)
  |> Accelerate.map (Lib.Common.homogeneousToPoint)

-- | Turns points in screen-space to pixels in a 2D 'histogram' representation
-- where every pixel counts how many points it contains.
screenToPixels :: Exp Int -> Exp Int -> Acc (Vector (Float, Float)) -> Acc (Matrix Int)
screenToPixels width height input = permute (+) zeros (mapping input) (ones input)
  where
    zeros :: Acc (Matrix Int)
    zeros = fill (index2 height width) 0
    ones :: Acc (Vector (Float, Float)) -> Acc (Vector Int)
    ones array = fill (shape array) 1
    mapping :: Acc (Vector (Float, Float)) -> Exp DIM1 -> Exp DIM2
    mapping array index = pointToPixel width height (array ! index)

-- | Currently a very simple (and not very beautiful) implementation,
-- where we use 'black' to indicate no points
-- and 'white' to indicate one or more points.
pixelsToColours :: Acc (Matrix Int) -> Acc (Matrix Word32)
pixelsToColours pixels =
  pixels
  |> Accelerate.map pixelToColour
  -- |> Accelerate.map HSL.toRGB
  |> Accelerate.map RGB.packRGB
  where
    pixelToColour :: Exp Int -> Exp (RGB Float)
    pixelToColour val =
      let
        val' =
          val
          |> fromIntegral
          |> (/ 256)
          |> sqrt
          |> sqrt
      in
        RGB.rgb val' val' val'
        |> RGB.clamp
    -- pixelToColour l = HSL.hsl 0 1.0 ((fromIntegral l) / 255.0)
    -- pixelToColour :: Exp Int -> Exp (HSL Float)
    -- pixelToColour pixel = Accelerate.cond (pixel Accelerate.== 0) black white

-- | Turns a point to a pixel coordinate using the given `width` and `height` as picture dimensions.
--
-- This uses the special `ignore` index for points/pixels that fall outside of the region of the picture.
--
pointToPixel :: Exp Int -> Exp Int -> Exp (Float, Float) -> Exp DIM2
pointToPixel width height (unlift -> (x, y)) =
  cond
  (isPointVisible (lift (x, y)))
  (index2 ypos xpos)
  (ignore)
  where
    xpos = x * (fromIntegral width)  |> Accelerate.floor
    ypos = y * (fromIntegral height) |> Accelerate.floor

-- | True if the given point will be visible on the screen
--
-- Works on screen-coordinates;
-- that is: tests if point is inside the unit box
-- (the half-open two-dimensional range `[(0, 0)..(1, 1))`).
isPointVisible :: Exp (Float, Float) -> Exp Bool
isPointVisible (unlift -> (x, y)) =
  x >= 0 && x < 1 &&
  y >= 0 && y < 1
