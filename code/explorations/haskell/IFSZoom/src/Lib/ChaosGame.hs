{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.ChaosGame
  ( point2Homogeneous
  , homogeneous2point
  , transformationFromSixtuple
  , chaosTransform
  ) where

import Pipe

import Data.Array.Accelerate
import Data.Array.Accelerate.Linear (V3(..), M33)
import Data.Array.Accelerate.Linear.Matrix ((!*))

import qualified Lib.MortonCode

point2Homogeneous :: Exp (Float, Float) -> Exp (V3 Float)
point2Homogeneous (unlift -> (x, y)) = lift ((V3 x y 1) :: V3 (Exp Float))

homogeneous2point :: Exp (V3 Float) -> Exp (Float, Float)
homogeneous2point (unlift -> V3 x y _) = lift ((x, y) :: (Exp Float, Exp Float))

transformationFromSixtuple :: Exp (Float, Float, Float, Float, Float, Float) -> Exp (M33 Float)
transformationFromSixtuple sixtuple =
  let
    (a, b, c, d, e, f) = unlift sixtuple :: (Exp Float, Exp Float, Exp Float, Exp Float, Exp Float, Exp Float)
    matrix :: M33 (Exp Float)
    matrix = (V3 (V3 a b e)
                 (V3 c d f)
                 (V3 0 0 1))
  in
    lift matrix


chaosTransform :: Exp (M33 Float) -> Exp (V3 Float) -> Exp (V3 Float)
chaosTransform matrix point = matrix !* point


-- | Picks a transformation from an array of transformations
-- based on the value of `current_point`
-- to finally transform `prev_point`.
--
-- A bit of a hack because Accelerate's `scanl` requires the same type
-- for both the element and the accumulator.
--
-- TODO: Use a faster transformation than the morton-one
-- to pick a random transformation based on a (Float, Float) input,
-- such as just e.g. concatenating the floats.
pointBasedTransform :: Acc (Vector (M33 Float)) -> Exp (Float, Float) -> Exp (Float, Float) -> Exp (Float, Float)
pointBasedTransform transformations current_point prev_point =
  let
    transformation =
      current_point
      |> (Lib.MortonCode.pointToMorton)
      |> pickTransformation transformations
  in
    prev_point
    |> point2Homogeneous
    |> chaosTransform transformation
    |> homogeneous2point

pickTransformation :: Acc (Vector (M33 Float)) -> Exp Word64 -> Exp (M33 Float)
pickTransformation transformations rngval =
  transformations !! index
  where
    index = (fromIntegral rngval) `mod` (size transformations)

-- | Given a matrix with random points
-- we will perform the 'chaos game', separately on each row.
-- So all points in column `0` will be initial points,
-- in column `1` will have one transformation applied,
-- in column `2` two transformations, etc.
chaosGame :: Acc (Matrix (Float, Float)) -> Acc (Vector (M33 Float)) -> Acc (Matrix (Float, Float))
chaosGame random_points transformations =
  random_points
  |> scanl (pointBasedTransform transformations) starting_point
  where
    starting_point = (0, 0) |> lift
