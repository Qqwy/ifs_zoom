{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IFSConfig
  ( main
  , read
  , IFS(..)
  , Transformation(..)
  , TransformationWithProbability(..)
  , transformationWithProbabilityToSixtuplePair
  , transformationToSixtuple
  , extractTransformations
  ) where

import Prelude hiding (read)
import Pipe
import qualified Lib.Transformation

import Dhall
import qualified GHC.Float

data Transformation = Transformation
  {a :: Double,
   b :: Double,
   c :: Double,
   d :: Double,
   e :: Double,
   f :: Double
  }
  deriving (Generic, Show)

instance FromDhall Transformation

data TransformationWithProbability = TransformationWithProbability
  {transformation :: Transformation,
   probability :: Double
  }
  deriving (Generic, Show)
-- makeLenses ''TransformationWithProbability
instance FromDhall TransformationWithProbability

data IFS = IFS
  { transformations :: [TransformationWithProbability]
  , initialCamera :: Transformation
  }
  deriving (Generic, Show)
-- makeLenses ''IFS
instance FromDhall IFS

main :: Text -> IO ()
main filename = do
    x <- input auto filename
    print (x :: IFS)

read :: Text -> IO IFS
read filename = do
  input auto filename

-- transformationToSixtuple :: Transformation -> (Double, Double, Double, Double, Double, Double)
-- transformationToSixtuple t = (t^.a, t^.b, t^.c, t^.d, t^.e, t^.f)

transformationWithProbabilityToSixtuplePair :: TransformationWithProbability -> ((Float, Float, Float, Float, Float, Float), Float)
transformationWithProbabilityToSixtuplePair TransformationWithProbability {transformation = t, probability = p} =
  (transformationToSixtuple t, d2f p)
  where
    d2f = GHC.Float.double2Float

transformationToSixtuple :: Transformation -> (Float, Float, Float, Float, Float, Float)
transformationToSixtuple (Transformation a b c d e f) =
  (d2f a, d2f b, d2f c, d2f d, d2f e, d2f f)
  where
    d2f = GHC.Float.double2Float

extractTransformations :: [TransformationWithProbability] -> [Lib.Transformation.Transformation]
extractTransformations transformations_list =
  transformations_list
  |> fmap transformationWithProbabilityToSixtuplePair
  |> fmap Lib.Transformation.fromSixtuplePair
  |> fmap fst
