module Types.Storage.Tracker where

import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import EulerHS.Prelude

data Tracker = Tracker
  { trip :: Trip,
    tracking :: Maybe Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
