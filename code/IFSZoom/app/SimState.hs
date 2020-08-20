{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TemplateHaskell     #-}
module SimState where

import Pipe
import qualified Data.Maybe

import Options(CLIOptions, HasCLIOptions(..))

import qualified IFSConfig
import IFSConfig(transformations)


import Lib.Common (Point)
import qualified Lib
import qualified Lib.Camera
import qualified Lib.ChaosGame
import qualified Lib.Guide
import Lib.Transformation (IFS)
import qualified Lib.Transformation

import Lens.Micro.Platform

import qualified Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Data.Array.Accelerate as Accelerate
import Data.Array.Accelerate(Z(..), (:.)(..))
import qualified Graphics.Gloss.Accelerate.Data.Picture
import qualified Data.Array.Accelerate.LLVM.PTX


import Input (Input, show_points, show_guides, zooming, translation, jump)
import qualified Input

data SimState = SimState
  { _picture :: !Lib.RasterPicture
  , _point_cloud :: Accelerate.Acc (Accelerate.Vector Point)
  , _should_update :: Bool
  , _dimensions :: (Word, Word)
  , _camera :: Lib.Camera
  , _input :: Input
  , _transformations_list :: [Lib.Transformation]
  , _initial_camera :: Lib.Camera
  , _jumps :: [Lib.Transformation]
  }

makeLenses ''SimState

initial :: IFSConfig.IFS -> CLIOptions -> Accelerate.Array Accelerate.DIM2 Accelerate.Word64 -> SimState
initial ifs_config options random_matrix =
  SimState
  { _picture = Accelerate.fromList (Z :. 0 :. 0) []
  , _should_update = True
  , _point_cloud = Lib.ChaosGame.chaosGame ifs n_points_per_thread paralellism' (Accelerate.use random_matrix)
  , _dimensions = (picture_width, picture_height)
  , _camera = Lib.Camera.identity |> Lib.Camera.absorbInitialCamera (Lib.Camera.inverse initial_camera)
  , _input = Input.initial
  , _transformations_list = transformations
  , _initial_camera = initial_camera
  , _jumps = []
  }
  where
    samples' = options ^. samples |> fromIntegral
    paralellism' = options ^. paralellism |> fromIntegral
    n_points_per_thread = samples' `div` paralellism'
    picture_width = options ^. render_width |> fromIntegral
    picture_height = options ^. render_height |> fromIntegral
    ifs = ifs_config |> IFSConfig.transformations |> buildIFS initial_camera
    transformations = ifs_config |> IFSConfig.transformations |> IFSConfig.extractTransformations initial_camera
    initial_camera = ifs_config |> IFSConfig.initialCamera |> IFSConfig.transformationToSixtuple |> Lib.Camera.fromSixtuple


-- | Updates the state of the sim_state based on earlier user input.
-- Runs once every frame
update :: Float -> SimState -> IO SimState
update _time_elapsed sim_state =
  case sim_state^.should_update of
    False ->
      return sim_state
    True -> do
      let
        new_sim_state =
          sim_state
          |> set picture (render sim_state)
          |> set should_update False

      putStrLn (show (sim_state^.camera))
      let cam = sim_state^.camera
      let res = map (Lib.Guide.isCameraInsideTransformation cam ) (sim_state^.transformations_list)
      putStrLn (show res)

      return new_sim_state

drawWithGuides :: SimState -> IO Gloss.Picture
drawWithGuides sim_state =
  [sim_state_picture, guides_picture]
  |> Graphics.Gloss.Data.Picture.pictures
  |> return
  where
    sim_state_picture =
      if sim_state^.input.show_points
      then draw sim_state
      else Graphics.Gloss.Data.Picture.blank
    guides_picture =
      if sim_state^.input.show_guides
      then Lib.Guide.drawGuides (sim_state^.camera) (sim_state^.initial_camera) (sim_state^.dimensions) (sim_state^.transformations_list)
      else Graphics.Gloss.Data.Picture.blank


-- | Called every frame.
-- When drawing, we simply return the picture we made earlier,
-- so we only re-render when the sim_state (camera position etc) changes
-- rather than every frame.
draw :: SimState -> Gloss.Picture
draw sim_state =
  sim_state^.picture
  |> (\picture -> Graphics.Gloss.Accelerate.Data.Picture.bitmapOfArray picture False)
  |> Graphics.Gloss.Data.Picture.scale 1 (-1) -- Gloss renders pictures upside-down https://github.com/tmcdonell/gloss-accelerate/issues/2

render :: SimState -> Lib.RasterPicture
render sim_state =
  sim_state^.point_cloud
  |> Lib.naivePointCloudToPicture camera' width height
  |> Data.Array.Accelerate.LLVM.PTX.run
  where
    (width, height) =
      sim_state^.dimensions
      |> over both (\val -> val |> fromIntegral |> Accelerate.unit)
    camera' =
      sim_state^.camera
      |> Lib.Camera.withInitial (sim_state^.initial_camera)
      |> Accelerate.lift
      |> Accelerate.unit

buildIFS :: Lib.Camera -> [IFSConfig.TransformationWithProbability] -> Accelerate.Acc IFS
buildIFS initial_camera transformations_list =
  transformations_list
  |> map IFSConfig.transformationWithProbabilityToSixtuplePair
  |> map Lib.Transformation.fromSixtuplePair
  |> map (over _1 (Lib.Camera.absorbInitialCamera initial_camera))
  |> Accelerate.fromList (Z :. (length transformations_list))
  |> Accelerate.use

applyDragging :: SimState -> SimState
applyDragging sim_state =
  let
    (screen_x, screen_y) = sim_state^.dimensions |> over both fromIntegral
    (tx, ty) = sim_state^.input.translation
    (unit_x, unit_y) = (tx / screen_x, ty / screen_y)
  in
    sim_state
    |> over camera (Lib.Camera.translate unit_x (-unit_y))
    |> set (input.translation) (0, 0)

applyZooming :: SimState -> SimState
applyZooming sim_state =
  let
    speed = 0.025
  in
    case sim_state^.input.zooming of
      Nothing ->
        sim_state
      Just Input.ZoomIn ->
        sim_state
        |> over camera (Lib.Camera.scale (1 + speed))
        |> set (input.zooming) Nothing
      Just Input.ZoomOut ->
        sim_state
        |> over camera (Lib.Camera.scale (1 - speed))
        |> set (input.zooming) Nothing

applyJumping :: SimState -> SimState
applyJumping sim_state =
  case sim_state^.input.jump of
    Nothing ->
      sim_state
    Just Input.JumpUp ->
      sim_state
      |> jumpUp
      |> set (input.jump) Nothing
    Just Input.JumpDown ->
      jumpDown sim_state
      |> set (input.jump) Nothing


jumpUp :: SimState -> SimState
jumpUp sim_state =
  case jumpTargets sim_state of
    [] ->
      sim_state
    (target : _) ->
      sim_state
      |> set camera (Lib.Transformation.combine [sim_state^.camera, Lib.Camera.inverse target])
      |> over jumps (target:)

jumpTargets :: SimState -> [Lib.Transformation]
jumpTargets sim_state =
  (sim_state^.transformations_list)
  |> filter (Lib.Guide.isCameraInsideTransformation (sim_state^.camera))
  |> map (\val -> Lib.Transformation.combine (reverse [Lib.Camera.inverse (sim_state^.initial_camera), Lib.Camera.inverse val, sim_state^.initial_camera]))

jumpDown :: SimState -> SimState
jumpDown sim_state =
      case sim_state^.jumps of
        [] ->
          sim_state
        (target:targets) ->
          sim_state
          |> set camera (Lib.Transformation.combine [sim_state^.camera, target])
          |> set jumps targets
