module SharedLogic.DriverPool (calculateDriverPool, recalculateDriverPool, getDriverPool) where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRideBooking
import qualified Storage.Queries.SearchReqLocation as QSReqLoc
import qualified Storage.Queries.TransporterConfig as QTConf
import Types.App (Driver)
import Types.Error
import qualified Types.Storage.Organization as SOrg
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchReqLocation as SSReqLoc
import qualified Types.Storage.TransporterConfig as STConf
import qualified Types.Storage.Vehicle as SV
import Utils.Common

driverPoolKey :: Id SRB.RideBooking -> Text
driverPoolKey = ("beckn:driverpool:" <>) . getId

getDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SRB.RideBooking ->
  m [Id Driver]
getDriverPool rideBookingId =
  Redis.getKeyRedis (driverPoolKey rideBookingId)
    >>= maybe calcDriverPool (pure . map Id)
  where
    calcDriverPool = do
      rideBooking <- QRideBooking.findById rideBookingId >>= fromMaybeM RideBookingDoesNotExist
      let vehicleVariant = rideBooking.vehicleVariant
          pickupPoint = rideBooking.fromLocationId
          orgId = rideBooking.providerId
      map (.driverId) <$> calculateDriverPool pickupPoint orgId (Just vehicleVariant)

recalculateDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SRB.RideBooking ->
  Id SOrg.Organization ->
  SV.Variant ->
  m [Id Driver]
recalculateDriverPool pickupPoint rideBookingId transporterId vehicleVariant = do
  driverPool <- map (.driverId) <$> calculateDriverPool pickupPoint transporterId (Just vehicleVariant)
  cancelledDrivers <- QRide.findAllCancelledByRBId rideBookingId <&> map (cast . (.driverId))
  let filteredDriverPool = filter (`notElem` cancelledDrivers) driverPool
  Redis.setExRedis (driverPoolKey rideBookingId) (getId <$> filteredDriverPool) (60 * 10)
  return filteredDriverPool

calculateDriverPool ::
  ( DBFlow m r,
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds]
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SOrg.Organization ->
  Maybe SV.Variant ->
  m [QP.DriverPoolResult]
calculateDriverPool locId orgId variant = do
  location <- QSReqLoc.findLocationById locId >>= fromMaybeM LocationNotFound
  let lat = location.lat
      long = location.lon
  radius <- getRadius
  measuringDurationToLog INFO "calculateDriverPool" $
    QP.getNearestDrivers
      (LatLong lat long)
      radius
      orgId
      variant
  where
    getRadius =
      QTConf.findValueByOrgIdAndKey orgId (STConf.ConfigKey "radius")
        >>= maybe
          (fromIntegral <$> asks (.defaultRadiusOfSearch))
          radiusFromTransporterConfig
    radiusFromTransporterConfig conf =
      fromMaybeM (InternalError "The radius is not a number.")
        . readMaybe
        . toString
        $ conf.value
