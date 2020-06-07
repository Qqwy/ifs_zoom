{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.ChaosGame
  ( chaosGame
  , fillChaosGameMatrix
  , pointToHomogeneous
  , homogeneousToPoint
  , transformationProbabilityFromSixtuplePair
  , transformationFromSixtuple
  , chaosTransform
  , floatPairToWord64
  , word64ToFloatPair
  ) where

import Pipe

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits ((.|.), shiftL, shiftR)
import Data.Array.Accelerate.Linear.Matrix ((!*))

import Lib.Common
import qualified Lib.Random


-- | Runs the chaos game
-- Given an array of transformations
-- a total number of points
-- and a seed
-- we'll return a long array of 2D-points.
chaosGame :: Acc IFS -> Int -> Int -> RNGVal -> Acc (Vector Point)
chaosGame transformations n_points_per_thread paralellism seed =
  Lib.Random.randomMatrix paralellism n_points_per_thread seed
  |> map word64ToFloatPair
  |> fillChaosGameMatrix transformations
  |> flatten

-- | Given a matrix with random points
-- we will perform the 'chaos game', separately on each row.
-- So all points in column `0` will be initial points,
-- in column `1` will have one transformation applied,
-- in column `2` two transformations, etc.
fillChaosGameMatrix :: Acc IFS -> Acc (Matrix Point) -> Acc (Matrix Point)
fillChaosGameMatrix transformations random_points =
  random_points
  |> prescanl (pointBasedTransform transformations) starting_point
  where
    starting_point = (0, 0) |> lift


-- | Transforms a single point using one of the IFS's transformations.
chaosTransform :: Exp Transformation -> Exp HomogeneousPoint -> Exp HomogeneousPoint
chaosTransform matrix point = matrix !* point


-- | Picks a transformation from an array of transformations
-- based on the value of `current_point`
-- to finally transform `prev_point`.
--
-- A bit of a hack because Accelerate's `scanl` requires the same type
-- for both the element and the accumulator.
--
pointBasedTransform :: Acc IFS -> Exp Point -> Exp Point -> Exp Point
pointBasedTransform transformations prev_point current_point =
  let
    transformation =
      current_point
      |> floatPairToWord64
      |> Lib.Random.extractRandomUnitIntervalDouble
      |> toFloating
      |> pickTransformation transformations
  in
    prev_point
    |> pointToHomogeneous
    |> chaosTransform transformation
    |> homogeneousToPoint

-- | Picks the correct transformation
-- based on the `rngval` passed in.
--
-- Assumes the probabilities that are the second elements of each of the `transformations`'s pairs
-- together sum to one.
--
-- `rngval` should be in the unit range [0..1)
-- (since we are comparing it with probabilities probability)
pickTransformation :: Acc IFS -> Exp Probability -> Exp Transformation
pickTransformation transformations rngval =
  transformations !! matching_transformation_index
  |> fst
  where
    matching_transformation_index =
      while checkLarger goToNext (lift (0, fraction))
      |> fst
    fraction = rngval
    checkLarger :: Exp (Int, Float) -> Exp Bool
    checkLarger (unlift -> (index, probability)) =
      probability > snd (transformations !! index)
    goToNext :: Exp (Int, Float) -> Exp (Int, Float)
    goToNext (unlift -> (index, probability)) =
     lift (index + 1, probability - snd (transformations !! index))


-- | A fast way to turn a (Float, Float) into a Word64.
--
-- The hope is that LLVM or the PTX compilers might optimize it to a no-op,
-- if the pair is adjacent in memory.
-- Do not confuse this with `pointToMorton`.
floatPairToWord64 :: Exp Point -> Exp Word64
floatPairToWord64 (unlift -> (x, y)) =
  let
    x' = x |> bitcast |> lift :: Exp Word32
    y' = y |> bitcast |> lift :: Exp Word32
  in
    (fromIntegral x' `shiftL` 32) .|. (fromIntegral y')

-- | Inverse of `floatPairToWord64`
word64ToFloatPair :: Exp Word64 -> Exp Point
word64ToFloatPair number =
  let
    x' = (number `shiftR` 32) |> fromIntegral :: Exp Word32
    x = x' |> bitcast :: Exp Float
    y' = number |> fromIntegral :: Exp Word32
    y = y' |> bitcast :: Exp Float
  in
    lift (x, y)
