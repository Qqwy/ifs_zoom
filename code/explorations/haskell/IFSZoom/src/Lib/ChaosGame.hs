{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.ChaosGame
  ( chaosGame
  , fillChaosGameMatrix
  , point2Homogeneous
  , homogeneous2point
  , transformationProbabilityFromSixtuplePair
  , transformationFromSixtuple
  , chaosTransform
  , floatPairToWord64
  , word64ToFloatPair
  ) where

import Pipe

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Bits ((.|.), shiftL, shiftR)
import Data.Array.Accelerate.Linear (V3(..), M33)
import Data.Array.Accelerate.Linear.Matrix ((!*))

import qualified Lib.Random

-- | Runs the chaos game
-- Given an array of transformations
-- a total number of points
-- and a seed
-- we'll return a long array of 2D-points.
chaosGame :: Acc (Vector (M33 Float, Float)) -> Int -> Int -> Word64 -> Acc (Vector (Float, Float))
chaosGame transformations n_points_per_thread paralellism seed =
  Lib.Random.randomMatrix paralellism n_points_per_thread seed
  |> map word64ToFloatPair
  -- |> map normalizeFloatPair
  |> fillChaosGameMatrix transformations
  |> flatten

-- | Given a matrix with random points
-- we will perform the 'chaos game', separately on each row.
-- So all points in column `0` will be initial points,
-- in column `1` will have one transformation applied,
-- in column `2` two transformations, etc.
fillChaosGameMatrix :: Acc (Vector (M33 Float, Float)) -> Acc (Matrix (Float, Float)) -> Acc (Matrix (Float, Float))
fillChaosGameMatrix transformations random_points =
  random_points
  |> prescanl (pointBasedTransform transformations) starting_point
  where
    starting_point = (0, 0) |> lift

point2Homogeneous :: Exp (Float, Float) -> Exp (V3 Float)
point2Homogeneous (unlift -> (x, y)) = lift ((V3 x y 1) :: V3 (Exp Float))

homogeneous2point :: Exp (V3 Float) -> Exp (Float, Float)
homogeneous2point (unlift -> V3 x y _) = lift ((x, y) :: (Exp Float, Exp Float))

transformationProbabilityFromSixtuplePair :: Exp ((Float, Float, Float, Float, Float, Float), Float) -> Exp (M33 Float, Float)
transformationProbabilityFromSixtuplePair (unlift -> (sixtuple, p)) =
  lift (transformationFromSixtuple sixtuple, (p :: Exp Float))

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

-- | TODO proper choice based on probability
--
-- Currently picks with equal probability
chaosTransform :: Exp (M33 Float) -> Exp (V3 Float) -> Exp (V3 Float)
chaosTransform matrix point = matrix !* point


-- | Picks a transformation from an array of transformations
-- based on the value of `current_point`
-- to finally transform `prev_point`.
--
-- A bit of a hack because Accelerate's `scanl` requires the same type
-- for both the element and the accumulator.
--
pointBasedTransform :: Acc (Vector (M33 Float, Float)) -> Exp (Float, Float) -> Exp (Float, Float) -> Exp (Float, Float)
pointBasedTransform transformations prev_point current_point =
  let
    transformation =
      current_point
      |> floatPairToWord64
      |> Lib.Random.extractRandomUnitIntervalDouble
      |> toFloating
      -- |> (\num -> (num `shiftR` 11) |> fromIntegral |> (* 9007199254740992))
      -- |> fst
      |> pickTransformation transformations
  in
    prev_point
    |> point2Homogeneous
    |> chaosTransform transformation
    |> homogeneous2point

-- | Picks the correct transformation
-- based on the `rngval` passed in.
--
-- Assumes the probabilities that are the second elements of each of the `transformations`'s pairs
-- together sum to one.
--
-- `rngval` might be any float; only its fractional part is considered
-- (since we are looking for a probability i.e. in the half-open range [0..1)).
pickTransformation :: Acc (Vector (M33 Float, Float)) -> Exp Float -> Exp (M33 Float)
pickTransformation transformations rngval =
  transformations !! matching_transformation_index
  |> fst
  where
    matching_transformation_index =
      while checkLarger goToNext (lift (0, fraction))
      |> fst
    -- fraction = rngval `mod'` 1 -- Turn this into a number in the half-open range [0..1)
    -- fraction = rngval `shiftR` 11 |> (* 9007199254740992)
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
floatPairToWord64 :: Exp (Float, Float) -> Exp Word64
floatPairToWord64 (unlift -> (x, y)) =
  let
    x' = x |> bitcast |> lift :: Exp Word32
    y' = y |> bitcast |> lift :: Exp Word32
  in
    (fromIntegral x' `shiftL` 32) .|. (fromIntegral y')

-- | Inverse of `floatPairToWord64`
word64ToFloatPair :: Exp Word64 -> Exp (Float, Float)
word64ToFloatPair number =
  let
    x' = (number `shiftR` 32) |> fromIntegral :: Exp Word32
    x = x' |> bitcast :: Exp Float
    y' = number |> fromIntegral :: Exp Word32
    y = y' |> bitcast :: Exp Float
  in
    lift (x, y)

-- normalizeFloatPair :: Exp (Float, Float) -> Exp (Float, Float)
-- normalizeFloatPair (unlift -> (x, y)) =
--   lift (x', y')
--   where
--     x' = (x :: Exp Float) `mod'` 1
--     y' = (y :: Exp Float) `mod'` 1
