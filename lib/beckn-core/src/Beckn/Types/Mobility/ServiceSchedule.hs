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
  { _id :: Text,
    _descriptor :: Maybe Descriptor,
    _location :: Location,
    _arrival_time :: StopTime,
    _departure_time :: StopTime,
    _transfers :: [Transfer],
    _status :: Text, -- ON-TIME, DELAYED, EARLY, CANCELLED, SKIP, RETURN
    _alt_timings :: [UTCTime],
    _alt_services :: [Text]
  }
  deriving (Generic, Show)

instance Example Schedule where
  example =
    Schedule
      { _id = idExample,
        _descriptor = example,
        _location = example,
        _arrival_time = example,
        _departure_time = example,
        _transfers = example,
        _status = "ON-TIME",
        _alt_timings = example,
        _alt_services = []
      }
