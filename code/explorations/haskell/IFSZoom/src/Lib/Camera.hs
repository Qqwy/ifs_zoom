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
  , defaultCamera
  ) where

import Pipe
import qualified Prelude
import Data.Array.Accelerate as Accelerate
import qualified Lib.Common

import Data.Array.Accelerate.Linear (V3(..), M33)
import Data.Array.Accelerate.Linear.Matrix ((!*))
import Linear.Matrix ((!*!), (!+!), inv33)

-- | A camera is 'just' a 2D affine transformation matrix.
type Camera = M33 Float


-- | Transforms a point from world space to screen space.
-- (Runs on the GPU!)
cameraTransform :: Exp Camera -> Exp (V3 Float) -> Exp (V3 Float)
cameraTransform camera point = camera !* point

-- | Inverts the transformation the camera makes.
-- useful for e.g. checking where the picure bounds (screen space) end up in world space.
-- inverseCamera :: Exp Camera -> Exp Camera
-- inverseCamera camera = inv33 camera

-- | Scales the camera in equal proportions using the given `scale` float.
scaleCamera :: Float -> Camera -> Camera
scaleCamera scale camera =
  scaleMatrix !*! camera
  where
    scaleMatrix = (V3
                    (V3 scale 0     0)
                    (V3 0     scale 0)
                    (V3 0     0     1)
                  )

-- | Translates the camera position using the given `horizontal` and `vertical` offsets.
translateCamera :: Float -> Float -> Camera -> Camera
translateCamera horizontal vertical camera =
  translationMatrix !+! camera
  where
    translationMatrix = (V3
                          (V3 0 0 horizontal)
                          (V3 0 0 vertical)
                          (V3 0 0 0)
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

defaultCamera :: Camera
defaultCamera =
  -- (1, 0, 0, 1, 0, 0)
  (((recip 11), 0, 0, -(recip 11), 0.5, 1))
  |> cameraFromSixtuple
