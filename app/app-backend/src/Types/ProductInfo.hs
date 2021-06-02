module Types.ProductInfo where

import Beckn.Utils.JSON
import EulerHS.Prelude
import Types.Common

data ProductInfo = ProductInfo
  { provider :: Maybe Provider,
    tracker :: Maybe Tracker
  }
  deriving (Generic, Show)

instance FromJSON ProductInfo where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ProductInfo where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Tracker = Tracker
  { trip :: Trip,
    tracking :: Maybe Tracking
  }
  deriving (Generic, Show)

instance FromJSON Tracker where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Tracker where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
