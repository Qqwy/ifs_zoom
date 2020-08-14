module Lib.Guide
  ( drawGuides
  , allCombinations
  , combinationsUpToDepth
  , isCameraInsideTransformation
  ) where

import Pipe
import qualified Lib
import qualified Lib.Common
import Lib.Common (Point)
import Lib.Transformation (Transformation)
import qualified Lib.Transformation
import qualified Lib.Geometry

import qualified Control.Applicative

import qualified Lib.Camera
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Picture
import qualified Linear.Matrix

type Guide = [Point]

drawGuides ::
  Lib.Camera          -- ^ The current camera matrix
  -> Lib.Camera       -- ^ The initial camera matrix
  -> (Word, Word)     -- ^ The screen dimensions
  -> [Transformation] -- ^List of transformations of this IFS
  -> Gloss.Picture
drawGuides camera initial_camera dimensions transformations =
  guides
  |> reverse -- Draw shallower guides on top
  |> Graphics.Gloss.Data.Picture.pictures
  where
    dims = dimensions |> (\(x, y) -> (fromIntegral x, fromIntegral y))
    colors = cycle guide_colors
    guides =
      transformations
      |> combinationsUpToDepth' 5
      |> (zip colors)
      |> map (\(color, transformations) -> map (\ts -> (color, ts)) transformations)
      |> concat
      |> map (\(color, transformations) -> drawGuide camera initial_camera dims transformations color)

guide_colors :: [Gloss.Color]
guide_colors = [Gloss.red, Gloss.green, Gloss.blue, Gloss.cyan, Gloss.magenta, Gloss.yellow]

buildGuide :: Lib.Camera -> Lib.Camera -> (Float, Float) -> [Transformation] -> Guide
buildGuide camera initial_camera dimensions transformations =
  unitGuide
  |> transformGuide (Lib.Camera.inverse initial_camera)
  |> transformGuide (Lib.Transformation.combine transformations)
  |> fmap (Lib.Camera.cameraTransform initial_camera)
  |> guideToScreen camera dimensions


drawGuide :: Lib.Camera -> Lib.Camera -> (Float, Float) -> [Transformation] -> Gloss.Color -> Gloss.Picture
drawGuide camera initial_camera dimensions transformations color =
  buildGuide camera initial_camera dimensions transformations
  |> guideToPicture dimensions
  |> Graphics.Gloss.Data.Picture.color (color |> Gloss.withAlpha 0.5)

unitGuide :: Guide
unitGuide =
  guideFromCoords (0, 0, 1, 1)

guideFromCoords :: (Float, Float, Float, Float) ->  Guide
guideFromCoords (x, y, w, h) =
  [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]

transformGuide :: Transformation -> Guide -> Guide
transformGuide transformation guide_points =
  guide_points
  |> fmap (Lib.Camera.cameraTransform transformation)

guideToScreen :: Lib.Camera -> (Float, Float) -> Guide -> Guide
guideToScreen camera (screen_width, screen_height) guide_points =
  guide_points
  |> fmap (Lib.Camera.cameraTransform camera)
  |> fmap (\(x, y) -> (x*screen_width, y*(-screen_height)))

guideToPicture :: (Float, Float) -> Guide -> Gloss.Picture
guideToPicture (screen_width, screen_height) guide_points =
  guide_points
  |> Graphics.Gloss.Data.Picture.line
  |> Graphics.Gloss.Data.Picture.translate (-screen_width/2) (screen_height/2)

-- | Returns all combinations of the elements in a list
-- This goes on forever.
-- Shorter combinations are first
--
-- ## Examples:
--
-- >>> take 4 $ allCombinations [1, 2, 3]
-- [[],[1],[2],[3]]
-- >>> take 13 $ allCombinations [1, 2, 3]
-- [[],[1],[2],[3],[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
allCombinations:: [a] -> [[a]]
allCombinations elems =
  elems
  |> allCombinations'
  |> concat

allCombinations' :: [a] -> [[[a]]]
allCombinations' elems =
  [[]]
  |> iterate (Control.Applicative.liftA2 (:) elems)

-- | Returns all combinations of the elements in a list
-- that are at most `depth` elements long
-- Shorter combinations are returned first.
--
-- ## Examples:
--
-- >>> combinationsUpToDepth 0 [1, 2, 3]
-- [[]]
-- >>> combinationsUpToDepth 1 [1, 2, 3]
-- [[],[1],[2],[3]]
-- >>> combinationsUpToDepth 2 [1, 2, 3]
-- [[],[1],[2],[3],[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
combinationsUpToDepth :: Int -> [a] -> [[a]]
combinationsUpToDepth depth elems =
  elems
  |> combinationsUpToDepth' (depth + 1)
  |> concat

combinationsUpToDepth' :: Int -> [a] -> [[[a]]]
combinationsUpToDepth' depth elems =
  elems
  |> allCombinations'
  |> take depth

-- | Checks whether the current camera is 'inside' a transformation
--
-- Here 'inside' means that the 'screen' transformed by the inverse camera matrix,
-- is completely contained in the 'screen' transformed by the transformation.
--
-- `initial_camera` is used to move between 'normalized screen coordinates'
-- (which `camera` uses)
-- and 'world coordinates'
-- (which `transformation` uses).
isCameraInsideTransformation :: Lib.Camera -> Lib.Camera -> Transformation -> Bool
isCameraInsideTransformation camera initial_camera transformation =
  Lib.Transformation.isInvertible transformation
  &&
  Lib.Geometry.isPolygonInsidePolygon transformation_coords camera_coords
    where
      camera_coords =
        unitGuide
        |> transformGuide (Lib.Camera.inverse camera)
        |> transformGuide (Lib.Camera.inverse initial_camera)
      transformation_coords =
        unitGuide
        |> transformGuide (Lib.Camera.inverse initial_camera)
        |> transformGuide transformation
