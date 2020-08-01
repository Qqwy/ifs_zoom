module Lib.Guide
  ( drawGuides
  ,  allCombinations
  , combinationsUpToDepth
  , isCameraInsideTransformation
  ) where

import Pipe
import qualified Lib
import qualified Lib.Common
import Lib.Common (Transformation, Point)
import qualified Lib.Geometry

import qualified Control.Applicative

import qualified Lib.Camera
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Picture
import qualified Linear.Matrix

import Debug.Trace

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
    colors = cycle [Gloss.red, Gloss.green, Gloss.blue, Gloss.cyan, Gloss.magenta, Gloss.yellow]
    guides =
      transformations
      |> combinationsUpToDepth' 2
      |> (zip colors)
      |> map (\(color, transformations) -> map (\ts -> (color, ts)) transformations)
      |> concat
      |> map (\(color, transformations) -> drawGuide camera initial_camera dims transformations color)

combineTransformations :: [Transformation] -> Transformation
combineTransformations multiple =
  foldr (Linear.Matrix.!*!) Lib.Common.identityTransformation multiple

buildGuide :: Lib.Camera -> Lib.Camera -> (Float, Float) -> [Transformation] -> [Point]
buildGuide camera initial_camera dimensions transformations =
  (0, 0, 1, 1)
  |> guideFromCoords initial_camera
  |> transformGuide (combineTransformations transformations)
  |> fmap (Lib.Camera.cameraTransform initial_camera)
  |> guideToScreen camera dimensions


drawGuide :: Lib.Camera -> Lib.Camera -> (Float, Float) -> [Transformation] -> Gloss.Color -> Gloss.Picture
drawGuide camera initial_camera dimensions transformations color =
  buildGuide camera initial_camera dimensions transformations
  |> guideToPicture dimensions
  |> Graphics.Gloss.Data.Picture.color (color |> Gloss.withAlpha 0.5)

guideFromCoords :: Lib.Camera ->(Float, Float, Float, Float) ->  [Point]
guideFromCoords initial_camera (x, y, w, h) =
  [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]
  |> fmap (Lib.Camera.cameraTransform (Lib.Camera.inverseCamera initial_camera))

transformGuide :: Transformation -> [Point] -> [Point]
transformGuide transformation guide_points =
  guide_points
  |> fmap (Lib.Camera.cameraTransform transformation)

guideToScreen :: Lib.Camera -> (Float, Float) -> [Point] -> [Point]
guideToScreen camera (screen_width, screen_height) guide_points =
  guide_points
  |> fmap (Lib.Camera.cameraTransform camera)
  |> fmap (\(x, y) -> (x*screen_width, y*(-screen_height)))

guideToPicture :: (Float, Float) -> [Point] -> Gloss.Picture
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


isCameraInsideTransformation :: Lib.Camera -> Lib.Camera -> Transformation -> Bool
isCameraInsideTransformation camera initial_camera transformation =
  if Linear.Matrix.det33 transformation == 0 then False
  else Lib.Geometry.isPolygonInsidePolygon transformation_coords camera_coords
  where
    camera_coords =
      -- buildGuide camera initial_camera (800, 800) []
      guideFromCoords (Lib.Common.identityTransformation) (0, 0, 1, 1)
      |> Prelude.fmap (Lib.Camera.cameraTransform (Lib.Camera.inverseCamera camera))
      |> Prelude.fmap (Lib.Camera.cameraTransform (Lib.Camera.inverseCamera initial_camera))
      |> traceShowId
    transformation_coords =
      -- buildGuide camera initial_camera (800, 800) [transformation]
      guideFromCoords initial_camera (0, 0, 1, 1)
      |> Prelude.fmap (Lib.Camera.cameraTransform transformation)
      |> traceShowId
  -- where
  --   unit_square =
  --     guideFromCoords initial_camera (0, 0, 1, 1)
  --   camera_coords =
  --     unit_square
  --     |> Prelude.fmap (Lib.Camera.cameraTransform (initial_camera))
  --     |> Prelude.fmap (Lib.Camera.cameraTransform (camera))
  --   transformation_coords =
  --     unit_square
  --     |> Prelude.fmap (Lib.Camera.cameraTransform transformation)
  --     |> Prelude.fmap (Lib.Camera.cameraTransform (initial_camera))
  --     |> Prelude.fmap (Lib.Camera.cameraTransform (camera))

-- guideFromCoords :: (Float, Float, Float, Float) ->  [Point]
-- guideFromCoords (x, y, w, h)  =
--   [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]

