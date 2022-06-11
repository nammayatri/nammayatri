module Domain.Types.DriverLocation where

import Beckn.Product.MapSearch.GoogleMaps
import Beckn.Types.Id
import Beckn.Types.MapSearch
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
  deriving (Generic, Show, Eq)

instance HasCoordinates DriverLocation where
  getCoordinates x = LatLong x.lat x.lon
