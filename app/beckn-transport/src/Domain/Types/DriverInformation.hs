module Domain.Types.DriverInformation where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import EulerHS.Prelude

data DriverInformation = DriverInformation
  { driverId :: Id Person,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchBack :: Bool
  }
  deriving (Generic)
