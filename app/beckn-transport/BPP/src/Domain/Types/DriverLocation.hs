module Domain.Types.DriverLocation where

import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates)
import Beckn.Types.Id
import Data.Time
import Domain.Types.Person (Person)
import EulerHS.Prelude hiding (id, state)

data DriverLocation = DriverLocation
  { driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates)
