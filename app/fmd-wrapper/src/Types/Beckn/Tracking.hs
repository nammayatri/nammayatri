module Types.Beckn.Tracking where

import Beckn.Utils.JSON
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Tracking = Tracking
  { url :: Maybe BaseUrl,
    status :: Maybe TrackingStatus
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data TrackingStatus = ACTIVE | INACTIVE
  deriving (Generic, Show)

instance FromJSON TrackingStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON TrackingStatus where
  toJSON = genericToJSON constructorsToLowerOptions
