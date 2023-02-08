module Domain.Types.DriverLocation where

import Data.Time
import Domain.Types.Person (Person)
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.HasCoordinates (HasCoordinates)
import Kernel.Types.Id

data DriverLocation = DriverLocation
  { driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    coordinatesCalculatedAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToJSON, FromJSON)
