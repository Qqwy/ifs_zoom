module Lib.CameraSpec (spec) where

import qualified Lib.Camera

import Pipe
import Test.Hspec
import Test.QuickCheck
import qualified Data.Array.Accelerate.Interpreter as Interpreter
import Data.Array.Accelerate
import qualified Linear.Epsilon


spec :: Spec
spec = do
  describe "Lib.Camera.scaleCamera" $ do
    it "is unchanged given scale `1`" $ do
      property $ \camera_tuple ->
        let
          camera =
           (camera_tuple :: (Float, Float, Float, Float, Float, Float))
           |> Lib.Camera.fromSixtuple
          res =
            camera
            |> Lib.Camera.scale 1
        in
          -- Check for 'subjective' floating-point 'equality'.
          Linear.Epsilon.nearZero ((res) Prelude.- (camera))
    it "can zoom in and then back out" $ do
      property $ \camera_tuple zoom_scale ->
        let
          camera =
           (camera_tuple :: (Float, Float, Float, Float, Float, Float))
           |> Lib.Camera.fromSixtuple
          res =
            camera
            |> Lib.Camera.scale zoom_scale
            |> Lib.Camera.scale (recip zoom_scale)
        in
          zoom_scale Prelude./= 0
          ==>
          -- Check for 'subjective' floating-point 'equality'.
          Linear.Epsilon.nearZero ((res) Prelude.- (camera))

  describe"Lib.Camera.translateCamera" $ do
    it "the identity-camera translates (0, 0) exactly where we want it" $ do
      property $ \horizontal vertical ->
        let
          camera =
            ((1,0,0,1,0,0) :: (Float, Float, Float, Float, Float, Float))
            |> Lib.Camera.fromSixtuple
            |> Lib.Camera.translate horizontal vertical
          res =
            ((0, 0) :: (Float, Float))
            |> lift
            |> (Lib.Camera.cameraTransformGPU (lift camera))
          in
          (runE res) Prelude.== (horizontal, vertical)

runE :: Elt e => Exp e -> e
runE expr = indexArray (Interpreter.run (unit expr)) Z
