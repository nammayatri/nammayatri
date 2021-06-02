module Beckn.Types.Mobility.Transfer where

-- import           Types.API.External.Mobility.Mode -- not availble in github
import Beckn.Types.Mobility.Route
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Transfer = Transfer
  { mode :: Text, -- "WALK", "SHUTTLE", "TRAVELATOR", "ELEVATOR", "ESCALATOR"
    route :: Route
  }
  deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Transfer where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Transfer where
  example =
    Transfer
      { mode = "WALK",
        route = example
      }
