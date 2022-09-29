module Domain.Types.DriverInformation where

import Beckn.Types.Id
import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import EulerHS.Prelude

data DriverInformation = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    optForRental :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool
  }
  deriving (Generic)
