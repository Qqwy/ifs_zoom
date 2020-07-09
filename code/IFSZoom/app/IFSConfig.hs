{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module IFSConfig (main) where


import Dhall

data Example = Example { foo :: Natural, bar :: Vector Double }
    deriving (Generic, Show)
instance FromDhall Example

data Transformation = Transformation {a :: Double, b :: Double, c :: Double, d :: Double, e :: Double, f :: Double}
  deriving (Generic, Show)
instance FromDhall Transformation

data IFS = IFS
  { transformations :: [(Transformation, Double)]
  , initialCamera :: Transformation
  }
  deriving (Generic, Show)
instance FromDhall IFS

main :: Text -> IO ()
main filename = do
    x <- input auto filename
    print (x :: IFS)
