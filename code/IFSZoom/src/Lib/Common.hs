{-# LANGUAGE ViewPatterns #-}
{-|
 Module      : Lib.ChaosGame
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

Contains common types, type aliases and functions
that are used throughout the application
-}
module Lib.Common
  ( Point
  , HomogeneousPoint
  , pointToHomogeneous
  , pointToHomogeneousGPU
  , homogeneousToPoint
  , homogeneousToPointGPU
  , mapPointAsHomogeneous
  , mapPointAsHomogeneousGPU
  ) where

import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Linear (V3(..))

-- | Since we are working in 2D, a point contaisn two coordinates.
type Point = (Float, Float)

-- | A point that we can easily transform using our `Transformation` type.
type HomogeneousPoint = V3 Float

-- | Turns a 2D (x, y) point into a 3D vector ('homogeneous notation') where the 'z' component is 1
pointToHomogeneous :: Point -> HomogeneousPoint
pointToHomogeneous (x, y) = V3 x y 1

-- | Identical to `pointToHomogeneous` but runs on the GPU
pointToHomogeneousGPU :: Exp Point -> Exp HomogeneousPoint
pointToHomogeneousGPU (unlift -> (x, y)) = lift ((V3 x y 1) :: V3 (Exp Float))

-- | Inverse of `pointToHomogeneous`
--
-- Note that `s` is not always `1`; it is altered by scaling transformations.
homogeneousToPoint :: HomogeneousPoint -> Point
homogeneousToPoint (V3 x y s) = (x / s, y / s)

-- | Identical to `homogeneousToPoint` but runs on the GPU
homogeneousToPointGPU :: Exp HomogeneousPoint -> Exp Point
homogeneousToPointGPU (unlift -> V3 x y s) = lift ((x / s, y / s) :: (Exp Float, Exp Float))

-- | Runs a function working on a point in homogeneous form on a 'normal' 2D point.
-- by wrapping it in a conversion to and from homogeneous notation
mapPointAsHomogeneous :: (HomogeneousPoint -> HomogeneousPoint) -> Point -> Point
mapPointAsHomogeneous fun point =
  point
  |> pointToHomogeneous
  |> fun
  |> homogeneousToPoint

-- | Identical to `mapPointAsHomogeneous` but runs on the GPU
mapPointAsHomogeneousGPU :: (Exp HomogeneousPoint -> Exp HomogeneousPoint) -> Exp Point -> Exp Point
mapPointAsHomogeneousGPU fun point =
  point
  |> pointToHomogeneousGPU
  |> fun
  |> homogeneousToPointGPU
