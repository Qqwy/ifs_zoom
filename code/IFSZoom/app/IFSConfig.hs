{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IFSConfig (main, read, IFS(..), Transformation(..), TransformationWithProbability(..), transformationWithProbabilityToSixtuplePair) where

import Prelude hiding (read)
import Dhall
import Lens.Micro.Platform
import GHC.Float (double2Float)

data Transformation = Transformation
  {a :: Double,
   b :: Double,
   c :: Double,
   d :: Double,
   e :: Double,
   f :: Double
  }
  deriving (Generic, Show)
-- makeLenses ''Transformation
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
transformationWithProbabilityToSixtuplePair TransformationWithProbability {transformation = Transformation a b c d e f, probability = p} =
  let
    d2f = double2Float
  in
    ((d2f a, d2f b, d2f c, d2f d, d2f e, d2f f), d2f p)
