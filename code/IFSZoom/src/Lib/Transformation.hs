{-# LANGUAGE ViewPatterns #-}
module Transformation
  ( Transformation
  , Probability
  , IFS
  , fromSixtuplePair
  , fromSixtuplePairGPU
  , fromSixtuple
  , fromSixtupleGPU
  , identity
  , isInvertible
  , transform
  , transformGPU
  , invert
  ) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Linear (V3(..), M33)
import qualified Linear.Matrix as Matrix
import qualified Data.Array.Accelerate.Linear.Matrix as GPU.Matrix

import Lib.Common (Point)
import qualified Lib.Common

-- | An affine transformation is manipulated in this program as its augmented matrix.
type Transformation = M33 Float
-- | Type that indicates that we are expecting a float in the [0..1) range.
type Probability = Float
-- | An Iterated Function System is described by a collection of transformations
-- with, for the Chaos Game, associated probabilities
type IFS = Vector (Transformation, Probability)

fromSixtuplePair :: ((Float, Float, Float, Float, Float, Float), Probability) -> (Transformation, Probability)
fromSixtuplePair ((sixtuple, p)) =
  (fromSixtuple sixtuple, (p :: Float))

fromSixtuple :: (Float, Float, Float, Float, Float, Float) -> Transformation
fromSixtuple sixtuple =
  let
    (a, b, c, d, e, f) = sixtuple :: (Float, Float, Float, Float, Float, Float)
    matrix :: M33 (Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    matrix


fromSixtuplePairGPU :: Exp ((Float, Float, Float, Float, Float, Float), Probability) -> Exp (Transformation, Probability)
fromSixtuplePairGPU (unlift -> (sixtuple, p)) =
  lift (fromSixtupleGPU sixtuple, (p :: Exp Float))

fromSixtupleGPU :: Exp (Float, Float, Float, Float, Float, Float) -> Exp Transformation
fromSixtupleGPU sixtuple =
  let
    (a, b, c, d, e, f) = unlift sixtuple :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    matrix :: M33 (Exp Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    lift matrix

identity :: Transformation
identity = Matrix.identity

isInvertible :: Transformation -> Bool
isInvertible t = Matrix.det33 t Prelude./= 0

transform :: Transformation -> Point -> Point
transform t p =
  Lib.Common.mapPointAsHomogeneous (transform' t) p

transform' :: Transformation -> (V3 Float) -> (V3 Float)
transform' t p = t Matrix.!* p

-- | Transforms a point from world space to screen space.
-- (Runs on the GPU!)
transformGPU :: Exp Transformation -> Exp Lib.Common.Point -> Exp Lib.Common.Point
transformGPU t p =
  Lib.Common.mapPointAsHomogeneousGPU (transformGPU' t) p

transformGPU' :: Exp Transformation -> Exp (V3 Float) -> Exp (V3 Float)
transformGPU' t p = t GPU.Matrix.!* p


invert :: Transformation -> Maybe Transformation
invert t =
  if isInvertible t
  then
    Just (Matrix.inv33 t)
  else
    Nothing
