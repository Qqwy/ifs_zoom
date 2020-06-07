{-# LANGUAGE ViewPatterns #-}

module Lib.Camera
  ( Camera
  , scaleCamera
  , translateCamera
  , cameraFromSixtuple
  , cameraTransform
  ) where

import Pipe
import qualified Prelude
import Data.Array.Accelerate as Accelerate
import qualified Lib.Common

import Data.Array.Accelerate.Linear (V3(..), M33)
import Data.Array.Accelerate.Linear.Matrix ((!*), (!*!), (!+!), inv33)

-- | A camera is 'just' a 2D affine transformation matrix.
type Camera = M33 Float

cameraFromSixtuple :: Exp (Float, Float, Float, Float, Float, Float) -> Exp Camera
cameraFromSixtuple sixtuple = Lib.Common.transformationFromSixtuple sixtuple

-- | Transforms a point from world space to screen space.
cameraTransform :: Exp Camera -> Exp (V3 Float) -> Exp (V3 Float)
cameraTransform camera point = camera !* point

-- | Inverts the transformation the camera makes.
-- useful for e.g. checking where the picure bounds (screen space) end up in world space.
inverseCamera :: Exp Camera -> Exp Camera
inverseCamera camera = inv33 camera

-- | Scales the camera in equal proportions using the given `scale` float.
scaleCamera :: Exp Float -> Exp Camera -> Exp Camera
scaleCamera scale camera =
  scaleMatrix !*! camera
  where
    scaleMatrix = lift (V3
                   (V3 scale 0     0)
                   (V3 0     scale 0)
                   (V3 0     0     1))

-- | Translates the camera position using the given `horizontal` and `vertical` offsets.
translateCamera :: Exp Float -> Exp Float -> Exp Camera -> Exp Camera
translateCamera horizontal vertical camera =
  translationMatrix !+! camera
  where
    translationMatrix = lift (V3
                             (V3 0 0 horizontal)
                             (V3 0 0 vertical)
                             (V3 0 0 0)
                             )
