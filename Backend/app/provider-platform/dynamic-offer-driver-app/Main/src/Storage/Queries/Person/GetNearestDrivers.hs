module Storage.Queries.Person.GetNearestDrivers (getNearestDrivers, NearestDriversResult (..)) where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Mb
import qualified Data.Text.Encoding as TE
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Vehicle as DV
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestDriversResult = NearestDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.Variant,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDrivers ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m) =>
  Maybe Variant ->
  LatLong ->
  Meters ->
  Id Merchant ->
  Bool ->
  Maybe Seconds ->
  m [NearestDriversResult]
getNearestDrivers mbVariant fromLocLatLong radiusMeters merchantId onlyNotOnRide mbDriverPositionInfoExpiry = do
  startTime1 <- getCurrentTime
  driverLocs <- Int.getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry fromLocLatLong radiusMeters
  endTime1 <- getCurrentTime
  void $ L.runKVDB meshConfig.kvRedis $ L.lpush (TE.encodeUtf8 "DRIVER_LOCS_KEY") [show (diffUTCTime endTime1 startTime1)]
  startTime2 <- getCurrentTime
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) onlyNotOnRide (not onlyNotOnRide)
  endTime2 <- getCurrentTime
  void $ L.runKVDB meshConfig.kvRedis $ L.lpush (TE.encodeUtf8 "DRIVER_INFOS_KEY") [show (diffUTCTime endTime2 startTime2)]
  startTime3 <- getCurrentTime
  vehicle <- Int.getVehicles driverInfos
  endTime3 <- getCurrentTime
  void $ L.runKVDB meshConfig.kvRedis $ L.lpush (TE.encodeUtf8 "VEHICLES_KEY") [show (diffUTCTime endTime3 startTime3)]
  startTime4 <- getCurrentTime
  drivers <- Int.getDrivers vehicle
  endTime4 <- getCurrentTime
  void $ L.runKVDB meshConfig.kvRedis $ L.lpush (TE.encodeUtf8 "DRIVERS_KEY") [show (diffUTCTime endTime4 startTime4)]
  logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  let res = linkArrayList driverLocs driverInfos vehicle drivers
  logDebug $ "GetNearestDrivers Result:- " <> show (length res)
  return (makeNearestDriversResult =<< res)
  where
    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode) -> [NearestDriversResult]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dist, dlat, dlon, variant, mode) = do
      case mbVariant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant == SUV
              sedanResult = getResult SEDAN $ variant == SEDAN || (variant == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant == HATCHBACK || ((variant == SUV || variant == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant == TAXI_PLUS
              taxiResult = getResult TAXI $ variant == TAXI || ((variant == TAXI_PLUS || variant == SUV || variant == SEDAN || variant == HATCHBACK) && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestDriversResult (cast personId) mbDeviceToken mblang onRide (roundToIntegral dist) var dlat dlon mode | cond]

    linkArrayList driverLocations driverInformations vehicles persons =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
       in mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap) driverLocations

    buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      let dist = realToFrac $ distanceBetweenInMeters fromLocLatLong $ LatLong {lat = location.lat, lon = location.lon}
      if Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
        || ( case mbVariant of
               Just SEDAN ->
                 info.canDowngradeToSedan
                   && vehicle.variant == SUV
               Just HATCHBACK ->
                 info.canDowngradeToHatchback
                   && (vehicle.variant == SUV || vehicle.variant == SEDAN)
               Just TAXI ->
                 info.canDowngradeToTaxi
                   && (vehicle.variant == TAXI_PLUS || vehicle.variant == HATCHBACK || vehicle.variant == SEDAN || vehicle.variant == SUV)
               _ -> False
           )
        then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, dist, location.lat, location.lon, vehicle.variant, info.mode)
        else Nothing
