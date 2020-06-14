{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Lib.BinarySearchTree
  (
  ) where

import Pipe
import Data.Array.Accelerate

import Lib.Common


-- | The top-left and bottom-right bounds of the area, respectively.
type Bounds = ((Float, Float), (Float, Float))

-- | A collection of bounds.
-- This BST is expected to be 2^n elements large,
-- allowing us to know exactly how many children each element has
-- without storing extra data.
--
-- Note that the BST will not store the points itself,
-- since those only need half the storage
-- (the lower and upper bound are the same for a single point)
type BinarySearchTree = Vector Bounds


binarySearchTree :: Acc (Vector Point) -> Acc BinarySearchTree
binarySearchTree points = undefined

oneBSTLayer :: Acc (Vector Bounds) -> Acc (Vector Bounds)
oneBSTLayer prev_layer =
  prev_layer
  |> arrayReshape (Z :. (size `div` 2) :. 2)
  |> fold1 computeBounds
  where
    computeBounds (unlift -> (l1, h1)) (unlift -> (l2, h2)) =
      (min l1 l2, max h1 h2)
  --   indexes = generate (index1 size prev_layer) (\index -> index `div` 2)
