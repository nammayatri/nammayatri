module Domain.Types.DriverStats where

import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Id

data DriverStats = DriverStats
  { driverId :: Id Driver,
    idleSince :: UTCTime
  }
  deriving (Generic)
