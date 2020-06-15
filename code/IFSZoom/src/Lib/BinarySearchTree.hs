{-# LANGUAGE ViewPatterns #-}

module Lib.BinarySearchTree
  ( BinarySearchTree
  , binarySearchTree
  , oneBSTLayer
  , traverseBST
  , inspectNodes
  , parentsToChildren
  ) where

import Pipe
import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Either

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

-- | Reduces the BST to return only the points that are useful
-- for the current camera angle
--
-- this means:
-- - points outside the viewport are discarded early
-- - BST nodes smaller than a pixel don't need further expansion
--
-- TODO currently returns a datatype that is not the final thing.
traverseBST :: Acc (Scalar Bounds) -> Acc BinarySearchTree -> Acc (Vector Int, Vector Int)
traverseBST camera_bounds bst =
  -- Implementation: 
  -- Loop over `(work, result)`
  -- at every step accumulating more in `result`
  -- and mapping values in `work` to their children.
  awhile nodesToBeVisited (inspectNodes camera_bounds bst) (use (initial_work, initial_result))
  where
    initial_result = fromList (Z :. 0) []
    initial_work = fromList (Z :. 1) [0]
    nodesToBeVisited :: (Acc (Vector Int, Vector Int)) -> Acc (Scalar Bool)
    nodesToBeVisited input =
      let
        work = input |> unlift |> afst :: Acc (Vector Int)
      in
      unit (size work > (constant 0))

inspectNodes :: Acc (Scalar Bounds) -> Acc BinarySearchTree -> (Acc (Vector Int, Vector Int)) -> (Acc (Vector Int, Vector Int))
inspectNodes camera_bounds bst (unlift -> (prev_work, prev_result)) =
    lift (next_work, prev_result ++ next_result)
  where
    nodes' = prev_work |> map inspectNode :: Acc (Vector (Either Int Int))
    next_work = nodes' |> rights |> afst |> parentsToChildren :: Acc (Vector Int)
    next_result = nodes' |> lefts |> afst :: Acc (Vector Int)
    inspectNode :: Exp Int -> Exp (Either Int Int)
    inspectNode node =
      if True then
        -- visit children
        right (node * 2)
      else
        -- No more visiting necessary
        left 0

-- | Maps parent-indices
-- to their children
--
-- for every `i` in the input array
-- the output array will contain the subsequent elements `i*2, i*2+1`
parentsToChildren :: Acc (Vector Int) -> Acc (Vector Int)
parentsToChildren nodes =
  nodes
  |> replicate (constant (Z :. All :. (2 :: Int)))
  |> imap parentToChildIndex
  |> flatten
  where
    parentToChildIndex :: Exp DIM2 -> Exp Int -> Exp Int
    parentToChildIndex (unindex2 -> indexes) val =
      let
        (index, child) = unlift indexes :: (Exp Int, Exp Int)
      in
        cond (child == 0) (val * 2) (val * 2 + 1)
