module Beckn.Types.Mobility.Stop where

import Beckn.Types.Core.Location
import Data.Text
import Data.Time
import EulerHS.Prelude

data Stop = Stop
  { _location :: Location,
    _arrival_time :: LocalTime,
    _departure_time :: LocalTime
  }
  deriving (Generic, Show)

instance FromJSON Stop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Stop where
  toJSON = genericToJSON stripLensPrefixOptions
