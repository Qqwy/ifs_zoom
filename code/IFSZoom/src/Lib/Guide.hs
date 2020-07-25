module Lib.Guide
  (
    drawGuides
  ) where

import Pipe
import qualified Lib
import qualified Lib.Common
import Lib.Common (Transformation)
import qualified Lib.Camera
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Picture

drawGuides :: Lib.Camera -> Lib.Camera -> (Word, Word) ->  Gloss.Picture
drawGuides camera initial_camera dimensions =
  drawGuide camera initial_camera dims []
  |> Graphics.Gloss.Data.Picture.color Gloss.red
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
