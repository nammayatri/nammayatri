module Storage.Queries.Allocation where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.ProductInstance as QPI
import qualified Types.Storage.Person as SP
import qualified Types.Storage.ProductInstance as SPI
import qualified Types.Storage.Vehicle as SV

assignDriver ::
  [Id SPI.ProductInstance] ->
  SV.Vehicle ->
  SP.DecryptedPerson ->
  DB.SqlDB ()
assignDriver piIdList vehicle driver = do
  QPI.updateVehicle piIdList (Just $ vehicle.id)
  QPI.updateDriver piIdList (Just personId)
  QDI.updateOnRide driverId True
  QPI.updateStatusByIds piIdList SPI.TRIP_ASSIGNED
  where
    personId = driver.id
    driverId = cast personId
