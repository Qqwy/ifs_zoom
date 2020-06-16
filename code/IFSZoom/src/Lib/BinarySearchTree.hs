{-# LANGUAGE ViewPatterns #-}

module Lib.BinarySearchTree
  ( BinarySearchTree
  , Bounds
  , binarySearchTree
  , oneBSTLayer
  , traverseBST
  , inspectNodes
  , parentsToChildren
  , depth
  , numContainedPoints
  , log2
  ) where

import Pipe
import qualified Prelude as P
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Either

import Lib.Common

-- | A collection of bounds.
-- This BST is expected to be 2^n elements large,
-- allowing us to know exactly how many children each element has
-- without storing extra data.
--
-- Note that the BST will not store the points itself,
-- since those only need half the storage
-- (the lower and upper bound are the same for a single point)
type BinarySearchTree = Vector Bounds

type BSTIndex = Int

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
binarySearchTree bst_depth points =
  applications
  where
    applications =
      points
      |> firstBSTLayer
      |> P.iterate oneBSTLayer
      |> P.take bst_depth
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
traverseBST :: Exp Bounds -> Acc BinarySearchTree -> Acc (Vector Point) -> Acc (Vector (Point, Int))
traverseBST camera_bounds bst points =
  -- Implementation:
  -- Loop over `(work, result)`
  -- at every step accumulating more in `result`
  -- and mapping values in `work` to their children.
  (use (initial_work, initial_result))
  |> awhile nodesToBeVisited (inspectNodes camera_bounds bst points)
  |> asnd
  |> map finalTransform
  where
    initial_result = fromList (Z :. 0) []
    initial_work = fromList (Z :. 1) [0]
    nodesToBeVisited :: (Acc (Vector Int, Vector Int)) -> Acc (Scalar Bool)
    nodesToBeVisited input =
      let
        work = input |> unlift |> afst :: Acc (Vector Int)
      in
      unit (size work > (constant 0))
    finalTransform :: Exp Int -> Exp (Point, Int)
    finalTransform node_index =
      let
        location = node_index |> lookupNode bst points |> fst -- at this time bounds are always smaller than a pixel so we only need a single point
        population = numContainedPoints bst node_index
      in
        lift (location, population)

inspectNodes :: Exp Bounds -> Acc BinarySearchTree -> Acc (Vector Point) -> (Acc (Vector Int, Vector Int)) -> (Acc (Vector Int, Vector Int))
inspectNodes camera_bounds bst points (unlift -> (prev_work, prev_result)) =
    lift (next_work, prev_result ++ next_result)
  where
    nodes' = prev_work |> map (inspectNode camera_bounds bst points) :: Acc (Vector (Either BSTIndex BSTIndex))
    next_work = nodes' |> rights |> afst |> parentsToChildren  :: Acc (Vector Int)
    next_result = nodes' |> lefts |> afst |> filterUnimportantNodes :: Acc (Vector Int)
    filterUnimportantNodes :: Acc (Vector BSTIndex) -> Acc (Vector BSTIndex)
    filterUnimportantNodes nodes = nodes |> filter (> 0) |> afst

-- | checks if a node is in bounds.
--
-- returns:
-- `Left 0` if the node is out of bounds. This node can be completely filtered without affecting the rsesult.
-- `Left idx` (where `idx` is positive) if there is no need to iterate deeper (because we've reached a single point or (TODO) possibly are smaller than individual pixels).
-- `Right idx` if the node is in bounds and we need to look at its children.
inspectNode :: Exp Bounds -> Acc BinarySearchTree -> Acc (Vector Point) -> Exp Int -> Exp (Either BSTIndex BSTIndex)
inspectNode camera_bounds bst points node_index =
  cond (nodeIsInBounds camera_bounds node) (
    cond (isPoint node) (
      left node_index
    )(
      right node_index
    )
  )(
      left (0)
   )
  -- cond (not (isPoint node) && nodeIsInBounds camera_bounds node) (
  --   -- visit children
  --   right node_index
  -- ) (
  --   -- No more visiting necessary
  --   -- return amount of points contained in node
  --   -- TODO does not strip 'discardable' points
  --   left node_index
  -- )
  where
    node = lookupNode bst points node_index
    isPoint node = fst node == snd node

lookupNode :: Acc BinarySearchTree -> Acc (Vector Point) -> Exp Int -> Exp Bounds
lookupNode bst points node_index =
      cond (node_index < (size bst))
      (
        -- Internal node
        bst !! node_index
      )(
       -- leaf node, return single point as 'BST node'.
        points
        |> (!! (node_index - (size bst)))
        |> (\point -> lift (point, point))
       )

nodeIsInBounds :: Exp Bounds -> Exp Bounds -> Exp Bool
nodeIsInBounds camera_bounds bounds =
  let
    ((clx, cly), (chx, chy)) = unliftBound camera_bounds :: ((Exp Float, Exp Float), (Exp Float, Exp Float))
    ((blx, bly), (bhx, bhy)) = unliftBound bounds :: ((Exp Float, Exp Float), (Exp Float, Exp Float))
  in
    (blx <= chx && clx <= bhx)
    &&
    (bly <= chy && cly <= bhy)

unliftBound :: Exp Bounds -> ((Exp Float, Exp Float), (Exp Float, Exp Float))
unliftBound (unlift -> (p1, p2)) = (unliftPoint p1, unliftPoint p2)


-- | Maps parent-indices
-- to their children
--
-- for every `i` in the input array
-- the output array will contain the consecutive elements `i*2, i*2+1`
--
-- ## Examples
--
-- >>> import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- >>> arr = use $ fromList (Z :. 5) ([0, 3..]) :: Acc (Vector Int)
-- >>> CPU.run $ Lib.BinarySearchTree.parentsToChildren arr
-- Vector (Z :. 10) [1, 2, 7, 8, 13, 14, 19, 20, 25, 26]
parentsToChildren :: Acc (Vector BSTIndex) -> Acc (Vector BSTIndex)
parentsToChildren nodes =
  nodes
  |> replicate (constant (Z :. All :. (2 :: Int)))
  |> imap parentToChildIndex
  |> flatten
  where
    parentToChildIndex :: Exp DIM2 -> Exp Int -> Exp Int
    parentToChildIndex (unindex2 -> indexes) val =
      let
        (_index, child) = unlift indexes :: (Exp Int, Exp Int)
      in
        cond (child == 0) (val * 2 + 1) (val * 2 + 2)

-- | Returns the depth of a particular Binary Search Tree
depth :: Acc BinarySearchTree -> Exp Int
depth bst =
  bst
  |> size
  |> log2
  where

-- | Helper function calculating the _floored_ base-2 logarithm on integer inputs.
log2 :: Exp Int -> Exp Int
log2 num =
  num
  |> fromIntegral
  |> (logBase (constant 2) :: Exp Float -> Exp Float)
  |> floor

-- | the number of points contained in a node at a particular `node_index`.
--
-- When `node_index` is larger than the BST size,
-- it will return `1`, assuming that an index for a single point (a 'leaf node') was passed.
numContainedPoints :: Acc BinarySearchTree -> Exp Int -> Exp Int
numContainedPoints bst node_index =
  cond (node_index >= (size bst))
  (
    1
  ) (
    2^node_height
  )
  where
    node_height = log2 ((size bst) - node_index) + 1
