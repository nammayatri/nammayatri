module Beckn.Types.Mobility.Stop where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Mobility.Transfer
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude

data Stop = Stop
  { _id :: Text,
    _descriptor :: Maybe Descriptor,
    _location :: Location,
    _arrival_time :: StopTime,
    _departure_time :: StopTime,
    _transfers :: [Transfer]
  }
  deriving (Generic, Show)

instance FromJSON Stop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Stop where
  toJSON = genericToJSON stripLensPrefixOptions

data StopTime = StopTime
  { _est :: UTCTime,
    _act :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON StopTime where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON StopTime where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example StopTime where
  example =
    StopTime
      { _est = example,
        _act = example
      }

instance Example Stop where
  example =
    Stop
      { _id = idExample,
        _descriptor = example,
        _location = example,
        _arrival_time = example,
        _departure_time = example,
        _transfers = example
      }
