{-# LANGUAGE ViewPatterns #-}

module Lib.BinarySearchTree
  ( BinarySearchTree
  , binarySearchTree
  , oneBSTLayer
  ) where

import Pipe
import qualified Prelude as P
import Data.Array.Accelerate

import Lib.Common


-- | The top-left and bottom-right bounds of the area, respectively.
type Bounds = (Point, Point)

-- | A collection of bounds.
-- This BST is expected to be 2^n elements large,
-- allowing us to know exactly how many children each element has
-- without storing extra data.
--
-- Note that the BST will not store the points itself,
-- since those only need half the storage
-- (the lower and upper bound are the same for a single point)
type BinarySearchTree = Vector Bounds

-- | Builds a binary search tree bottom-up.
-- Note that this function takes `depth` which is a power of two.
-- It uses metaprogramming to create a program that combines
-- exactly 2^depth points together
-- forming a BST with `(2^depth)/2 - 1` internal nodes.
--
-- The node at index 0 is the root
-- index (1, 2) its children
-- etc
--
-- In general, children of a node at index `i` àre at indexes `i*2` and `i*2+1`.
binarySearchTree :: Int -> Acc (Vector Point) -> Acc BinarySearchTree
binarySearchTree depth points =
  applications
  where
    applications =
      points
      |> firstBSTLayer
      |> P.iterate oneBSTLayer
      |> P.take depth
      |> P.tail
      |> P.foldl1 (\lower upper -> upper ++ lower)

-- | For the first layer
-- the lower and upper bound are the same.
-- Note that this first layer is not itself stored
-- (because that would be wasteful)
-- but only kicks off the creation
-- of the next layers of the BST.
firstBSTLayer :: Acc (Vector Point) -> Acc BinarySearchTree
firstBSTLayer points =
  points
  |> map (\point -> lift (point, point))

-- | Combines all consecutive two bounds together
-- to form the next higher layer of the BST.
oneBSTLayer :: Acc (Vector Bounds) -> Acc (Vector Bounds)
oneBSTLayer prev_layer =
  prev_layer
  |> reshape newshape
  |> fold1 computeBounds
  where
    newshape = index2 (size prev_layer `div` 2) 2
    computeBounds :: Exp Bounds -> Exp Bounds -> Exp Bounds
    computeBounds b1 b2 =
      let
        (l1, h1) = unlift b1 :: (Exp Point, Exp Point)
        (l2, h2) = unlift b2 :: (Exp Point, Exp Point)
      in
        lift (min l1 l2, max h1 h2)

