module Lib.CameraSpec (spec) where

import qualified Lib.Camera
import qualified Lib.ChaosGame

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
           |> lift
           |> Lib.Camera.cameraFromSixtuple
          res =
            camera
            |> Lib.Camera.scaleCamera 1
        in
          (runE res) Prelude.== (runE camera)
    it "can zoom in and then back out" $ do
      property $ \camera_tuple zoom_scale ->
        let
          camera =
           (camera_tuple :: (Float, Float, Float, Float, Float, Float))
           |> lift
           |> Lib.Camera.cameraFromSixtuple
          res =
            camera
            |> Lib.Camera.scaleCamera (constant zoom_scale)
            |> Lib.Camera.scaleCamera (recip (constant zoom_scale))
        in
          zoom_scale Prelude./= 0
          ==>
          -- Check for 'subjective' floating-point 'equality'.
          Linear.Epsilon.nearZero ((runE res) Prelude.- (runE camera))

  describe"Lib.Camera.translateCamera" $ do
    it "the identity-camera translates (0, 0) exactly where we want it" $ do
      property $ \horizontal vertical ->
        let
          camera =
            ((1,0,0,1,0,0) :: (Float, Float, Float, Float, Float, Float))
            |> lift
            |> Lib.Camera.cameraFromSixtuple
            |> Lib.Camera.translateCamera (constant horizontal) (constant vertical)
          res =
            ((0, 0) :: (Float, Float))
            |> lift
            |> Lib.ChaosGame.pointToHomogeneous
            |> Lib.Camera.cameraTransform camera
            |> Lib.ChaosGame.homogeneousToPoint
          in
          (runE res) Prelude.== (horizontal, vertical)

runE :: Elt e => Exp e -> e
runE expr = indexArray (Interpreter.run (unit expr)) Z
