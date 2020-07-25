module Lib.Guide
  (
    drawGuides
  ) where

import Pipe
import qualified Lib
import qualified Lib.Camera
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Picture

drawGuides :: Lib.Camera -> Lib.Camera -> (Word, Word) -> Gloss.Picture
drawGuides camera initial_camera dimensions =
  drawGuide (0, 0, 1, 1) camera initial_camera dims
  |> Graphics.Gloss.Data.Picture.color Gloss.red
  where
    dims = dimensions |> (\(x, y) -> (fromIntegral x, fromIntegral y))

drawGuide :: (Float, Float, Float, Float) -> Lib.Camera -> Lib.Camera -> (Float, Float) -> Gloss.Picture
drawGuide (x, y, w, h) camera initial_camera (pw, ph) =
  -- Graphics.Gloss.Data.Picture.blank
  [(x, y), (x+w, y), (x+w, y+h), (x, y+h), (x, y)]
  |> fmap (Lib.Camera.cameraTransform (Lib.Camera.inverseCamera initial_camera))
  |> fmap (Lib.Camera.cameraTransform camera)
  |> fmap (\(x, y) -> (x*pw, y*(-ph)))
  |> Graphics.Gloss.Data.Picture.line
  |> Graphics.Gloss.Data.Picture.translate (-pw/2) (ph/2)
