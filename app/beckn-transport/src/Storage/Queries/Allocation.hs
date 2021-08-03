module Storage.Queries.Allocation where

import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Ride as QRide
import qualified Types.Storage.OldRide as SRide
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Vehicle as SV

assignDriver ::
  Id SRide.Ride ->
  SV.Vehicle ->
  SP.DecryptedPerson ->
  DB.SqlDB ()
assignDriver rideId vehicle driver = do
  QRide.updateVehicle rideId (Just $ vehicle.id)
  QRide.updateDriver rideId (Just personId)
  QDI.updateOnRide driverId True
  QRide.updateStatus rideId SRide.TRIP_ASSIGNED
  where
    personId = driver.id
    driverId = cast personId
