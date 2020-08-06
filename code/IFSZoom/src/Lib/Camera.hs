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
  , identity
  -- , isCameraInsideTransformation
  ) where

import Pipe
import Data.Array.Accelerate as Accelerate
import Lib.Common (Point)
import Lib.Transformation (Transformation)
import qualified Lib.Transformation

import Data.Maybe
import Data.Array.Accelerate.Linear (V3(..), M33)
import qualified Linear.Matrix as Matrix

-- | A camera is 'just' another transformation
type Camera = Transformation

-- | Transforms a point from world space to screen space.
cameraTransform :: Camera -> Point -> Point
cameraTransform =
  -- Lib.Common.mapPointAsHomogeneous (cameraTransform' camera) point
  Lib.Transformation.transform


-- | Transforms a point from world space to screen space.
-- (Runs on the GPU!)
cameraTransformGPU :: Exp Camera -> Exp Lib.Common.Point -> Exp Lib.Common.Point
cameraTransformGPU = Lib.Transformation.transformGPU

-- | Inverts the transformation the camera makes.
-- useful for e.g. checking where the picure bounds (screen space) end up in world space.
inverseCamera :: Camera -> Camera
inverseCamera camera =
  camera
  |> Lib.Transformation.invert
  |> Data.Maybe.fromJust


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


withInitialCamera :: Camera -> Camera -> Camera
withInitialCamera initial_camera camera =
  camera Matrix.!*! initial_camera

identity = Lib.Transformation.identity
