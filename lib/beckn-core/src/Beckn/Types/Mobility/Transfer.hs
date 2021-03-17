module Beckn.Types.Mobility.Transfer where

-- import           Types.API.External.Mobility.Mode -- not availble in github
import Beckn.Types.Mobility.Route
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Transfer = Transfer
  { _mode :: Text, -- "WALK", "SHUTTLE", "TRAVELATOR", "ELEVATOR", "ESCALATOR"
    _route :: Route
  }
  deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Transfer where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example Transfer where
  example =
    Transfer
      { _mode = "WALK",
        _route = example
      }
