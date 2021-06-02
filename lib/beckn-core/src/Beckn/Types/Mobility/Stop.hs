module Beckn.Types.Mobility.Stop where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Mobility.Transfer
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude

data Stop = Stop
  { id :: Text,
    descriptor :: Maybe Descriptor,
    location :: Location,
    arrival_time :: StopTime,
    departure_time :: StopTime,
    transfers :: [Transfer]
  }
  deriving (Generic, Show)

instance FromJSON Stop where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Stop where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data StopTime = StopTime
  { est :: UTCTime,
    act :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON StopTime where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON StopTime where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example StopTime where
  example =
    StopTime
      { est = example,
        act = example
      }

instance Example Stop where
  example =
    Stop
      { id = idExample,
        descriptor = example,
        location = example,
        arrival_time = example,
        departure_time = example,
        transfers = example
      }
