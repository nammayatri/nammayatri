module Domain.Types.DriverInformation where

import Data.Time (UTCTime)
import Domain.Types.Person (Person)
import EulerHS.Prelude
import Kernel.Types.Id

data DriverInformation = DriverInformation
  { driverId :: Id Person,
    adminId :: Maybe (Id Person),
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    optForRental :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool
  }
  deriving (Generic, ToJSON, FromJSON)
