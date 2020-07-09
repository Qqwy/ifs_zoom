{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module IFSConfig (main, read, IFS(..), Transformation(..), TransformationWithProbability(..), transformationWithProbabilityToSixtuplePair, transformations, initialCamera, transformation, probability) where

import Prelude hiding (read)
import Dhall
import Lens.Micro.Platform
import GHC.Float (double2Float)

data Example = Example { foo :: Natural, bar :: Vector Double }
    deriving (Generic, Show)
instance FromDhall Example

data Transformation = Transformation {_a :: Double, _b :: Double, _c :: Double, _d :: Double, _e :: Double, _f :: Double}
  deriving (Generic, Show)
makeLenses ''Transformation
instance FromDhall Transformation

data TransformationWithProbability = TransformationWithProbability {_transformation :: Transformation, _probability :: Double }
  deriving (Generic, Show)
makeLenses ''TransformationWithProbability
instance FromDhall TransformationWithProbability

data IFS = IFS
  { _transformations :: [TransformationWithProbability]
  , _initialCamera :: Transformation
  }
  deriving (Generic, Show)
makeLenses ''IFS
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
transformationWithProbabilityToSixtuplePair tp =
  let
    t = tp ^.transformation
    d2f = double2Float
  in
    ((d2f (t^.a), d2f (t^.b), d2f (t^.c), d2f (t^.d), d2f (t^.e), d2f (t^.f)), d2f (tp^.probability))
