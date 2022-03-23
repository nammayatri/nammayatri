module SharedLogic.DriverPool (calculateDriverPool, recalculateDriverPool, getDriverPool) where

import Beckn.External.GoogleMaps.Types (HasGoogleMaps)
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Tools.Metrics.Types
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (LatLong))
import qualified Beckn.Types.MapSearch as GoogleMaps
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
  ( CoreMetrics m,
    DBFlow m r,
    HasFlowEnv m r '["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    HasGoogleMaps m r c
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
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r c
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
    HasFlowEnv m r ["defaultRadiusOfSearch" ::: Meters, "driverPositionInfoExpiry" ::: Maybe Seconds],
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Id SSReqLoc.SearchReqLocation ->
  Id SOrg.Organization ->
  Maybe SV.Variant ->
  m [QP.DriverPoolResult]
calculateDriverPool locId orgId variant = do
  pickupLoc <- QSReqLoc.findLocationById locId >>= fromMaybeM LocationNotFound
  let pickupLatLong = LatLong pickupLoc.lat pickupLoc.lon
  radius <- getRadius
  approxDriverPool <-
    measuringDurationToLog INFO "calculateDriverPool" $
      QP.getNearestDrivers
        pickupLatLong
        radius
        orgId
        variant
  filterOutDriversWithDistanceAboveThreshold radius pickupLatLong approxDriverPool
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

filterOutDriversWithDistanceAboveThreshold ::
  ( DBFlow m r,
    CoreMetrics m,
    HasGoogleMaps m r c
  ) =>
  Integer ->
  LatLong ->
  [QP.DriverPoolResult] ->
  m [QP.DriverPoolResult]
filterOutDriversWithDistanceAboveThreshold threshold pickupLatLong driverPoolResults = do
  getDistanceResults <- GoogleMaps.getDistances (Just GoogleMaps.CAR) originLatLongList [pickupLatLong] Nothing
  return $ filter (filterFunc getDistanceResults) driverPoolResults
  where
    originLatLongList =
      map (\dpRes -> LatLong dpRes.lat dpRes.lon) driverPoolResults
    filterFunc getDistanceResults dpRes = do
      let threshold' = fromIntegral threshold
      let mbGdRes = findGetDistanceResult getDistanceResults dpRes
      case mbGdRes of
        Nothing -> False
        Just gdRes -> gdRes.info.distance <= threshold'

findGetDistanceResult ::
  [GoogleMaps.GetDistanceResult] ->
  QP.DriverPoolResult ->
  Maybe GoogleMaps.GetDistanceResult
findGetDistanceResult getDistanceResults driverPoolResult =
  find filterFunc getDistanceResults
  where
    filterFunc gdRes = do
      case gdRes.origin of
        GoogleMaps.Location origin ->
          origin.lat == driverPoolResult.lat
            && origin.lng == driverPoolResult.lon
        _ -> False
