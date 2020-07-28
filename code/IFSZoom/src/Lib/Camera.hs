{-# LANGUAGE ViewPatterns #-}

{-|
 Module      : Lib.Camera
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

transformations to (groups of) points to turn them from world-space to screen-space.

The camera (a transformation matrix) itself is meant to be manipulated on the CPU.
The resulting transformation is then given to the GPU to map over all (visible) points.
 -}

module Lib.Camera
  ( Camera
  , scaleCamera
  , translateCamera
  , cameraFromSixtuple
  , cameraTransform
  , cameraTransformGPU
  , inverseCamera
  , defaultCamera
  , withInitialCamera
  ) where

import Pipe
import qualified Prelude
import Data.Array.Accelerate as Accelerate
import Lib.Common (Transformation, Point)
import qualified Lib.Common
import qualified Lib.Geometry

import Data.Array.Accelerate.Linear (V3(..), M33)
import qualified Data.Array.Accelerate.Linear.Matrix as GPU.Matrix
import qualified Linear.Matrix as Matrix

-- | A camera is 'just' a 2D affine transformation matrix.
type Camera = M33 Float

-- | Transforms a point from world space to screen space.
cameraTransform :: Camera -> Point -> Point
cameraTransform camera point =
  Lib.Common.mapPointAsHomogeneous (cameraTransform' camera) point

cameraTransform' :: Camera -> (V3 Float) -> (V3 Float)
cameraTransform' camera point = camera Matrix.!* point


-- | Transforms a point from world space to screen space.
-- (Runs on the GPU!)
cameraTransformGPU :: Exp Camera -> Exp Lib.Common.Point -> Exp Lib.Common.Point
cameraTransformGPU camera point =
  Lib.Common.mapPointAsHomogeneousGPU (cameraTransformGPU' camera) point

cameraTransformGPU' :: Exp Camera -> Exp (V3 Float) -> Exp (V3 Float)
cameraTransformGPU' camera point = camera GPU.Matrix.!* point


-- | Inverts the transformation the camera makes.
-- useful for e.g. checking where the picure bounds (screen space) end up in world space.
inverseCamera :: Camera -> Camera
inverseCamera camera = Matrix.inv33 camera


-- | Scales the camera in equal proportions using the given `scale` float.
--
-- Scales to the middle of the screen.
scaleCamera :: Float -> Camera -> Camera
scaleCamera scale camera =
  camera
  |> translateCamera (-0.5) (-0.5)
  |> scaleCamera' scale
  |> translateCamera (0.5) (0.5)

-- Performs the actual scaling
scaleCamera' :: Float -> Camera -> Camera
scaleCamera' scale camera =
  scaleMatrix Matrix.!*! camera
  where
    scaleMatrix =
      (V3
        (V3 1 0 0)
        (V3 0 1 0)
        (V3 0 0 (recip scale))
      )

-- | Translates the camera position using the given `horizontal` and `vertical` offsets.
translateCamera :: Float -> Float -> Camera -> Camera
translateCamera horizontal vertical camera =
  translationMatrix Matrix.!*! camera
  where
    translationMatrix = (V3
                          (V3 1 0 horizontal)
                          (V3 0 1 vertical)
                          (V3 0 0 1)
                        )

cameraFromSixtuple :: (Float, Float, Float, Float, Float, Float) -> Camera
cameraFromSixtuple (a, b, c, d, e, f) =
  let
    matrix :: M33 Float
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    matrix

defaultCamera :: (Float, Float, Float, Float, Float, Float) -> Camera
defaultCamera transformation_sixtuple =
  cameraFromSixtuple transformation_sixtuple

isCameraInsideTransformation :: Camera -> Transformation -> Bool
isCameraInsideTransformation camera transformation =
  Lib.Geometry.isPolygonInsidePolygon camera_coords transformation_coords
  where
    unit_square =
      guideFromCoords (0, 0, 1, 1)
    camera_coords =
      Prelude.fmap (cameraTransform camera) unit_square
    transformation_coords =
      Prelude.fmap (cameraTransform transformation) unit_square

guideFromCoords :: (Float, Float, Float, Float) ->  [Point]
guideFromCoords (x, y, w, h)  =
  [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]

withInitialCamera :: Camera -> Camera -> Camera
withInitialCamera initial_camera camera =
  -- (inverseCamera initial_camera) Matrix.!*! camera
  camera Matrix.!*! initial_camera
