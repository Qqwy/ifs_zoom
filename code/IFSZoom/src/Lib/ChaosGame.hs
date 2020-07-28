{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-|
 Module      : Lib.ChaosGame
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : experimental
 Portability : non-portable (GHC extensions)

Parallel 'Chaos Game' implementation.
The Chaos Game is a probabilistic algorithm to draw an Iterated Function System.
Curiously, given enough points, the contraction of the IFS will ensure that the result will converge
to the same deterministic picture.

This implementation of the Chaos Game takes a description of an IFS as input,
performs all random number generation and point-transforming on the GPU itself,
and finally returns a single vector of points as output.

This vector might be kept on the GPU for further manipulation (e.g. drawing pictures from them) afterwards.

Note that many 'internal' functions of this module have also been exported,
to make testing the various components easier.
 -}

module Lib.ChaosGame
  ( chaosGame
  , fillChaosGameMatrix
  , pointToHomogeneous
  , homogeneousToPoint
  , transformationProbabilityFromSixtuplePairGPU
  , transformationFromSixtupleGPU
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
--
-- Note that this implementation runs on a two-dimensional matrix:
-- each row is handled by a different thread
-- each field in each row is the next iteration of the chaos game.
--
-- Essentially this means that for a high number of `n_points_per_thread`
-- we'll have very high-quality points.
-- whereas for a high `paralellism` we'll be able to run the chaosGame faster.
--
-- So depending on the particular IFS and GPU, you'll want to tune those values differently.
chaosGame :: Acc IFS -- ^ A description of the IFS' transformations + probabilities
          -> Int -- ^ the amount of points to compute on each GPU thread.
          -> Int -- ^ the number of threads to spin up.
          -> Acc (Matrix Word64) -- ^ A random matrix
  -> Acc (Vector Point) -- ^ A representation of the resulting point cloud.
chaosGame transformations n_points_per_thread paralellism random_matrix =
  random_matrix
  -- Lib.Random.randomMatrix paralellism n_points_per_thread seed
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
-- this is why we transform from a pair of 32-bit floats to a single 64-bit integer (and back).
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
    mapPointAsHomogeneousGPU (chaosTransform transformation) prev_point

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
