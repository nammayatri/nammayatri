module Domain.Types.DriverStats where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Driver)

data DriverStats = DriverStats
  { driverId :: Id Driver,
    idleSince :: UTCTime
  }
  deriving (Generic)
