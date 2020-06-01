{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.ChaosGame
  ( chaosGame
  , fillChaosGameMatrix
  , point2Homogeneous
  , homogeneous2point
  , transformationFromSixtuple
  , chaosTransform
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
chaosGame :: Acc (Vector (M33 Float)) -> Int -> Word64 -> Acc (Vector (Float, Float))
chaosGame transformations n_points seed =
  Lib.Random.randomMatrix n_points n_points seed
  |> map word64ToFloatPair
  |> fillChaosGameMatrix transformations
  |> flatten

-- | Given a matrix with random points
-- we will perform the 'chaos game', separately on each row.
-- So all points in column `0` will be initial points,
-- in column `1` will have one transformation applied,
-- in column `2` two transformations, etc.
fillChaosGameMatrix :: Acc (Vector (M33 Float)) -> Acc (Matrix (Float, Float)) -> Acc (Matrix (Float, Float))
fillChaosGameMatrix transformations random_points =
  random_points
  |> scanl (pointBasedTransform transformations) starting_point
  where
    starting_point = (0, 0) |> lift

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
pointBasedTransform :: Acc (Vector (M33 Float)) -> Exp (Float, Float) -> Exp (Float, Float) -> Exp (Float, Float)
pointBasedTransform transformations current_point prev_point =
  let
    transformation =
      current_point
      |> (floatPairToWord64)
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
    (fromIntegral x') .|. (fromIntegral y' `shiftL` 32)

-- | Inverse of `floatPairToWord64`
word64ToFloatPair :: Exp Word64 -> Exp (Float, Float)
word64ToFloatPair number =
  let
    x' = number |> fromIntegral :: Exp Word32
    x = x' |> bitcast :: Exp Float
    y' = (number `shiftR` 32) |> fromIntegral :: Exp Word32
    y = y' |> bitcast :: Exp Float
  in
    lift (x, y)
