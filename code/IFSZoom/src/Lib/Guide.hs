module Lib.Guide
  (
    drawGuides,
    allCombinations,
    combinationsUpToDepth
  ) where

import Pipe
import qualified Lib
import qualified Lib.Common
import Lib.Common (Transformation)
import qualified Lib.Camera
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Picture

drawGuides ::
  Lib.Camera          -- ^ The current camera matrix
  -> Lib.Camera       -- ^ The initial camera matrix
  -> (Word, Word)     -- ^ The screen dimensions
  -> [Transformation] -- ^List of transformations of this IFS
  -> Gloss.Picture
drawGuides camera initial_camera dimensions transformations =
  drawGuide camera initial_camera dims []
  |> Graphics.Gloss.Data.Picture.color (Gloss.red |> Gloss.withAlpha 0.5)
  where
    dims = dimensions |> (\(x, y) -> (fromIntegral x, fromIntegral y))

drawGuide :: Lib.Camera -> Lib.Camera -> (Float, Float) -> [Transformation] -> Gloss.Picture
drawGuide camera initial_camera dimensions transformations =
  (0, 0, 1, 1)
  |> guideFromCoords initial_camera
  |> guideToPicture camera dimensions

guideFromCoords :: Lib.Camera ->(Float, Float, Float, Float) ->  [(Float, Float)]
guideFromCoords initial_camera (x, y, w, h)  =
  [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]
  |> fmap (Lib.Camera.cameraTransform (Lib.Camera.inverseCamera initial_camera))

guideToPicture :: Lib.Camera -> (Float, Float) -> [(Float, Float)] -> Gloss.Picture
guideToPicture camera (screen_width, screen_height) guide =
  guide
  |> fmap (Lib.Camera.cameraTransform camera)
  |> fmap (\(x, y) -> (x*screen_width, y*(-screen_height)))
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
allCombinations :: [a] -> [[a]]
allCombinations elems =
  [[]]
  |> iterate (fun elems)
  |> concat
  where
    fun elems xs =
      [elem : x | elem <- elems, x <- xs]

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
  take (n_elems depth) (allCombinations elems)
   where
     -- There probably is a mathematically more satisfying
     -- way of writing this:
     n_elems 0 = 1
     n_elems d = (length elems) ^ d + (n_elems (d - 1))
