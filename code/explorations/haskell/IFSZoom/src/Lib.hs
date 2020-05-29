{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Lib
-- Copyright   : [2020] Wiebe-Marten Wijnja
-- License     : BSD3
--
-- Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Lib (

  dotp,
  binarySearch

) where

import Data.Array.Accelerate

-- | A simple vector inner product
--
dotp :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Scalar Int)
dotp xs ys = fold (*) 1 ( zipWith (+) xs ys)


binarySearch :: Acc (Vector Int) -> Acc (Scalar Int) -> Acc (Scalar Int)
binarySearch arr wrapped_target =
  let
    target = the wrapped_target
    index = fst $ binarySearch' arr target 0 (length arr - 1)
  in
    unit index

binarySearch' :: Acc (Vector Int) -> Exp Int -> Exp Int -> Exp Int -> Exp (Int, Int)
binarySearch' arr target left right =
  let
    initial :: Exp (Int, Int)
    initial = liftedPair left right

    check :: Exp (Int, Int) -> Exp Bool
    check pair =
      let
        (left, right) = unlift pair
      in
        (arr !! left) < (arr !! right)

    body :: Exp (Int, Int) -> Exp (Int, Int)
    body pair =
      let
        (left, right) = unlift pair
        middle = (left + right) `quot` 2
      in
        -- case (compare (arr !! middle) target :: Ordering) of
        --   LT ->
        --     liftedPair (middle + 1) right
        --   GT ->
        --     liftedPair left (middle - 1)
        --   EQ ->
        --     liftedPair middle middle
        caseof (compare (arr !! middle) target)
        [
          ((== constant LT), liftedPair (middle + 1) right),
          ((== constant GT), liftedPair left (middle - 1))
        ]
        (liftedPair middle middle)

  in
    while check body initial
  -- let
  --   check :: Exp (DIM1, DIM1) -> Exp Bool
  --   check (left, right) = (arr ! left) < (arr ! right)
  -- in
  --   while _
  --   (\tuple ->
  --     let
  --       (left, right) = unlift tuple :: (Exp DIM1, Exp DIM1)
  --       middle :: Exp DIM1
  --       middle = (left + right) `div` 2
  --   in
  --       _
  --     -- lift $ case compare (arr ! middle) target of
  --     --   LT ->
  --     --     (middle + 1, right)
  --     --   GT ->
  --     --     (left, middle - 1)
  --     --   EQ ->
  --     --     (middle, middle)
  --     )
  --   (unlift (left, right))

-- arrsize :: Acc (Vector Int) -> Exp DIM1
-- arrsize arr = lift $ (\i -> Z :. i) $ length $ arr

liftedPair :: Exp Int -> Exp Int -> Exp (Int, Int)
liftedPair a b =
  let
    pair = (a, b) :: (Exp Int, Exp Int)
  in
    lift pair
