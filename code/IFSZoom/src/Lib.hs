{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Lib
-- Copyright   : [2020] Wiebe-Marten Wijnja
-- License     : BSD3
--
-- Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Lib
  ( naivePointCloudToPicture
  , Lib.Camera.Camera
  , Lib.Picture.RasterPicture
  , Lib.Transformation.Transformation
  , Lib.Transformation.IFS
) where

import Lib.Common
import qualified Lib.Camera
import qualified Lib.Picture
import qualified Lib.Transformation

import Data.Array.Accelerate


naivePointCloudToPicture :: Acc (Scalar Lib.Camera.Camera) -> Acc (Scalar Int) -> Acc (Scalar Int) -> Acc (Vector Point) -> Acc Lib.Picture.RasterPicture
naivePointCloudToPicture (the ->camera) (the -> width) (the -> height) point_cloud =
  Lib.Picture.naivePointCloudToPicture camera width height point_cloud
