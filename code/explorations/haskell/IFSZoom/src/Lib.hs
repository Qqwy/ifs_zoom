{-# LANGUAGE RebindableSyntax #-}
-- |
-- Module      : Lib
-- Copyright   : [2020] Wiebe-Marten Wijnja
-- License     : BSD3
--
-- Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Lib (

  dotp

) where

import Data.Array.Accelerate

-- | A simple vector inner product
--
dotp :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Scalar Double)
dotp xs ys = fold (*) 1 ( zipWith (+) xs ys)
