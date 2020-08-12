module Types.ProductInfo where

import EulerHS.Prelude
import Types.Common

data ProductInfo = ProductInfo
  { _provider :: Maybe Provider,
    _tracker :: Maybe Tracker
  }
  deriving (Generic, Show)

instance FromJSON ProductInfo where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProductInfo where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Tracker = Tracker
  { _trip :: Trip,
    _tracking :: Maybe Tracking
  }
  deriving (Generic, Show)

instance FromJSON Tracker where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tracker where
  toJSON = genericToJSON stripAllLensPrefixOptions
