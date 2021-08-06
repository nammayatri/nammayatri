module Beckn.Types.Mobility.Transfer where

-- import           Types.API.External.Mobility.Mode -- not availble in github
import Beckn.Types.Mobility.Route
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Transfer = Transfer
  { mode :: Text, -- "WALK", "SHUTTLE", "TRAVELATOR", "ELEVATOR", "ESCALATOR"
    route :: Route
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Transfer where
  example =
    Transfer
      { mode = "WALK",
        route = example
      }
