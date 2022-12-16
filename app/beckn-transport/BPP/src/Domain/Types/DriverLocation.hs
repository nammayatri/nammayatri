module Domain.Types.DriverLocation where

import Beckn.External.Maps.HasCoordinates (HasCoordinates)
import Beckn.Types.Id
import Data.Time
import Domain.Types.Person (Person)
import EulerHS.Prelude hiding (id, state)

data DriverLocation = DriverLocation
  { driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    coordinatesCalculatedAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToJSON, FromJSON)
