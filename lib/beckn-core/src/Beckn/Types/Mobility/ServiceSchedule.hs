module Beckn.Types.Mobility.ServiceSchedule where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Mobility.Stop (StopTime)
import Beckn.Types.Mobility.Transfer
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude

newtype ServiceSchedule = ServiceSchedule
  { _stops :: Schedule
  }
  deriving (Generic, Show)

data Schedule = Schedule
  { id :: Text,
    descriptor :: Maybe Descriptor,
    location :: Location,
    arrival_time :: StopTime,
    departure_time :: StopTime,
    transfers :: [Transfer],
    status :: Text, -- ON-TIME, DELAYED, EARLY, CANCELLED, SKIP, RETURN
    alt_timings :: [UTCTime],
    alt_services :: [Text]
  }
  deriving (Generic, Show)

instance Example Schedule where
  example =
    Schedule
      { id = idExample,
        descriptor = example,
        location = example,
        arrival_time = example,
        departure_time = example,
        transfers = example,
        status = "ON-TIME",
        alt_timings = example,
        alt_services = []
      }
