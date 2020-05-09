module Beckn.Types.Mobility.Transfer where

-- import           Types.API.External.Mobility.Mode -- not availble in github
import Beckn.Types.Mobility.Route
import Data.Text
import Data.Time
import EulerHS.Prelude

data Transfer = Transfer
  { _mode :: Text,
    _route :: Route
  }
  deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Transfer where
  toJSON = genericToJSON stripLensPrefixOptions
