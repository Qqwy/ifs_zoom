module Lib.SortSpec (spec) where

import qualified Data.List
import Pipe

import Test.Hspec
import Test.QuickCheck
import qualified Data.Array.Accelerate.Interpreter as Interpreter
-- import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX as PTX

import Data.Array.Accelerate as Accelerate
import qualified Lib.Sort

spec :: Spec
spec = do
  describe "Sort.radixSort" $ do
    it "returns a sorted list when passing in an unsorted list" $ do
      let
        sorted_arr = fromList (Z :. 10) ([0..]) :: Vector Word64
        unsorted_arr = Accelerate.reverse (use sorted_arr)

      Interpreter.run (Lib.Sort.radixSort unsorted_arr) `shouldBe` (sorted_arr)
    it "sorts arbitrary lists" $ do
      property $ \list ->
        let
          res :: [Word64]
          res =
            list
            |> fromList (Z :. Data.List.length list)
            |> use
            |> Lib.Sort.radixSort
            |> Interpreter.run
            |> toList
        in
          res Prelude.== (Data.List.sort list)
