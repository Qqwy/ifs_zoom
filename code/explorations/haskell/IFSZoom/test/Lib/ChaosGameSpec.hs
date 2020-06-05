module Lib.ChaosGameSpec (spec) where

import qualified Lib.ChaosGame

import Pipe

import Test.Hspec
import Test.QuickCheck
import qualified Data.Array.Accelerate.Interpreter as Interpreter

import Data.Array.Accelerate as Accelerate

spec :: Spec
spec = do
  describe "Lib.ChaosGame.floatPairToWord64/word64ToFloatPair" $ do
    it "are inverses 1" $  do
      property $ \floatpair ->
        let
          res =
            floatpair
            |> constant
            |> Lib.ChaosGame.floatPairToWord64
            |> Lib.ChaosGame.word64ToFloatPair
        in
          (runE res) Prelude.== floatpair
    it "are inverses 2" $  do
      property $ \word64 ->
        let
          res =
            word64
            |> constant
            |> Lib.ChaosGame.word64ToFloatPair
            |> Lib.ChaosGame.floatPairToWord64
        in
          (runE res) Prelude.== word64

runE :: Elt e => Exp e -> e
runE expr = indexArray (Interpreter.run (unit expr)) Z
