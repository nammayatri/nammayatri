module Fixtures.DriverLocation where

import Beckn.Types.Id
import qualified Domain.Types.DriverLocation as DDrLoc
import qualified Fixtures.Time as Fixtures

defaultDriverLocation :: DDrLoc.DriverLocation
defaultDriverLocation =
  DDrLoc.DriverLocation
    { driverId = Id "1",
      lat = 10,
      lon = 10,
      coordinatesCalculatedAt = Fixtures.defaultTime,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
