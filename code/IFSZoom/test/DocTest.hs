module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  let options = ["--verbose"]
  files <- glob "src/**/*.hs"
  doctest (options ++ files)
