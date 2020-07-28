module Lib.Geometry
  ( isPolygonInsidePolygon
  , isPointInsidePolygon
  ) where

import Lib.Common

-- | A polygon is a list of points
-- Note that for the functions in this module to work correctly,
-- we expect that you repeat the first point once more as final point.
--
-- So if you have a rectangle, you should supply a list of five points
-- where the first and final point are the same
type Polygon = [Point]

-- | Tests if `inner` is fully contained inside `outer`.
--
isPolygonInsidePolygon :: Polygon -> Polygon -> Bool
isPolygonInsidePolygon outer inner =
  all (isPointInsidePolygon outer) inner

-- | Tests whether a single 2D point is inside a (concave) polygon
--
-- Haskell implementation of the algorithm
-- described at https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
--
-- ## Examples:
--
-- >>> let rect = [(0, 0), (1, 0), (1, 1), (0, 1), (0, 0)]
-- >>> isPointInsidePolygon rect (0.5, 0.5)
-- True
--
-- >>> let rect = [(0, 0), (1, 0), (1, 1), (0, 1), (0, 0)]
-- >>> isPointInsidePolygon rect (1.5, 0.5)
-- False
--
isPointInsidePolygon :: Polygon -> Point -> Bool
isPointInsidePolygon = isPointInsidePolygon' False

isPointInsidePolygon' :: Bool -> [Point] -> Point ->  Bool
isPointInsidePolygon' _ [] _  = False -- A point cannot be in an empty polygon
isPointInsidePolygon' in_poly [_] _ = in_poly
isPointInsidePolygon' in_poly poly@((x1, y1) : (x2, y2) : _) point@(px, py) =
  if (y1 > py) /= (y2 > py)
     && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1)
  then
    isPointInsidePolygon' (not in_poly) (tail poly) point
  else
    isPointInsidePolygon' in_poly (tail poly) point
