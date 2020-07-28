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
  ( Transformation
  , Probability
  , IFS
  , Point
  , HomogeneousPoint
  , RNGVal
  , pointToHomogeneous
  , pointToHomogeneousGPU
  , homogeneousToPoint
  , homogeneousToPointGPU
  , mapPointAsHomogeneous
  , mapPointAsHomogeneousGPU
  , transformationProbabilityFromSixtuplePair
  , transformationFromSixtuple
  , transformationProbabilityFromSixtuplePairGPU
  , transformationFromSixtupleGPU
  , identityTransformation
  , isTransformationInvertible
  ) where

import Pipe
import Data.Array.Accelerate
import Data.Array.Accelerate.Linear (V3(..), M33)
import qualified Linear.Matrix

-- | An affine transformation is manipulated in this program as its augmented matrix.
type Transformation = M33 Float
-- | Type that indicates that we are expecting a float in the [0..1) range.
type Probability = Float
-- | An Iterated Function System is described by a collection of transformations
-- with, for the Chaos Game, associated probabilities
type IFS = Vector (Transformation, Probability)
-- | Since we are working in 2D, a point contaisn two coordinates.
type Point = (Float, Float)

-- | A point that we can easily transform using our `Transformation` type.
type HomogeneousPoint = V3 Float

-- | A seed or randomly chosen value.
type RNGVal = Word64

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

transformationProbabilityFromSixtuplePair :: ((Float, Float, Float, Float, Float, Float), Probability) -> (Transformation, Probability)
transformationProbabilityFromSixtuplePair ((sixtuple, p)) =
  (transformationFromSixtuple sixtuple, (p :: Float))

transformationFromSixtuple :: (Float, Float, Float, Float, Float, Float) -> Transformation
transformationFromSixtuple sixtuple =
  let
    (a, b, c, d, e, f) = sixtuple :: (Float, Float, Float, Float, Float, Float)
    matrix :: M33 (Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    matrix


transformationProbabilityFromSixtuplePairGPU :: Exp ((Float, Float, Float, Float, Float, Float), Probability) -> Exp (Transformation, Probability)
transformationProbabilityFromSixtuplePairGPU (unlift -> (sixtuple, p)) =
  lift (transformationFromSixtupleGPU sixtuple, (p :: Exp Float))

transformationFromSixtupleGPU :: Exp (Float, Float, Float, Float, Float, Float) -> Exp Transformation
transformationFromSixtupleGPU sixtuple =
  let
    (a, b, c, d, e, f) = unlift sixtuple :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    matrix :: M33 (Exp Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    lift matrix

identityTransformation :: Transformation
identityTransformation = Linear.Matrix.identity

isTransformationInvertible :: Transformation -> Bool
isTransformationInvertible t = Linear.Matrix.det33 t Prelude./= 0

