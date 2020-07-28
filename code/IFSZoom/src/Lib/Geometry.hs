module Lib.Geometry
  ( isPolygonInsidePolygon
  , isPointInsidePolygon
  ) where

import Pipe
import Lib.Common

isPolygonInsidePolygon :: [Point] -> [Point] -> Bool
isPolygonInsidePolygon inner outer =
  all (isPointInsidePolygon outer) inner

isPointInsidePolygon :: [Point] -> Point -> Bool
isPointInsidePolygon = isPointInsidePolygon' False

isPointInsidePolygon' :: Bool -> [Point] -> Point ->  Bool
isPointInsidePolygon' _ [] _  = False
isPointInsidePolygon' _ [_] _ = False
isPointInsidePolygon' in_poly poly@((x1, y1) : (x2, y2) : _) point@(px, py) =
  if (y1 > py) /= (y2 > py)
     && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1)
  then
    isPointInsidePolygon' (not in_poly) (tail poly) point
  else
    isPointInsidePolygon' in_poly (tail poly) point
