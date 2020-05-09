module Beckn.Types.External.Mobility.Tracking where
  
import           Data.Text
import           Data.Time
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Location

data Tracking =
  Tracking
    { _method :: Location -- "PULL", "PUSH"
    , _pull :: PullTrackingData
    }
      deriving (Generic, Show)

instance FromJSON Tracking where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tracking where
  toJSON = genericToJSON stripLensPrefixOptions

data PullTrackingData =
  PullTrackingData
    { _data_url :: Text
    , _embed_url :: Text
    }
      deriving (Generic, Show)

instance FromJSON PullTrackingData where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PullTrackingData where
  toJSON = genericToJSON stripLensPrefixOptions