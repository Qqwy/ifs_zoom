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
  , homogeneousToPoint
  , transformationProbabilityFromSixtuplePair
  , transformationFromSixtuple
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Linear (V3(..), M33)

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

pointToHomogeneous :: Exp Point -> Exp HomogeneousPoint
pointToHomogeneous (unlift -> (x, y)) = lift ((V3 x y 1) :: V3 (Exp Float))

homogeneousToPoint :: Exp HomogeneousPoint -> Exp Point
homogeneousToPoint (unlift -> V3 x y _) = lift ((x, y) :: (Exp Float, Exp Float))

transformationProbabilityFromSixtuplePair :: Exp ((Float, Float, Float, Float, Float, Float), Probability) -> Exp (Transformation, Probability)
transformationProbabilityFromSixtuplePair (unlift -> (sixtuple, p)) =
  lift (transformationFromSixtuple sixtuple, (p :: Exp Float))

transformationFromSixtuple :: Exp (Float, Float, Float, Float, Float, Float) -> Exp Transformation
transformationFromSixtuple sixtuple =
  let
    (a, b, c, d, e, f) = unlift sixtuple :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    matrix :: M33 (Exp Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    lift matrix
