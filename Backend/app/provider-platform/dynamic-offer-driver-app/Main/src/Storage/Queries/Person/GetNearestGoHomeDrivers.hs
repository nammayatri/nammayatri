module Storage.Queries.Person.GetNearestGoHomeDrivers
  ( getNearestGoHomeDrivers,
    NearestGoHomeDriversResult (..),
    NearestGoHomeDriversReq (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Mb
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Vehicle as DV
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.GenericPretty
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal as Int
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestGoHomeDriversReq = NearestGoHomeDriversReq
  { variant :: Maybe Variant,
    fromLocation :: LatLong,
    nearestRadius :: Meters,
    homeRadius :: Meters,
    merchantId :: Id Merchant,
    driverPositionInfoExpiry :: Maybe Seconds,
    isRental :: Bool
  }

data NearestGoHomeDriversResult = NearestGoHomeDriversResult
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

getNearestGoHomeDrivers ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) =>
  NearestGoHomeDriversReq ->
  m [NearestGoHomeDriversResult]
getNearestGoHomeDrivers NearestGoHomeDriversReq {..} = do
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocation nearestRadius
  driverHomeLocs <- Int.getDriverGoHomeReqNearby (driverLocs <&> (.driverId))
  driverInfos <- Int.getDriverInfosWithCond (driverHomeLocs <&> (.driverId)) True False isRental
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  let res = linkArrayList driverLocs driverInfos vehicle drivers
  logDebug $ "GetNearestGoHomeDrivers Result:- " <> show (length res)
  return (makeNearestGoHomeDriversResult =<< res)
  where
    makeNearestGoHomeDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Double, Variant, Maybe DriverInfo.DriverMode) -> [NearestGoHomeDriversResult]
    makeNearestGoHomeDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dist, dlat, dlon, variant', mode) = do
      case variant of
        Nothing -> do
          let autoResult = getResult AUTO_RICKSHAW $ variant' == AUTO_RICKSHAW
              suvResult = getResult SUV $ variant' == SUV
              sedanResult = getResult SEDAN $ variant' == SEDAN || (variant' == SUV && canDowngradeToSedan)
              hatchbackResult = getResult HATCHBACK $ variant' == HATCHBACK || ((variant' == SUV || variant' == SEDAN) && canDowngradeToHatchback)
              taxiPlusResult = getResult TAXI_PLUS $ variant' == TAXI_PLUS
              taxiResult = getResult TAXI $ variant' == TAXI || (variant' == TAXI_PLUS && canDowngradeToTaxi)
          autoResult <> suvResult <> sedanResult <> hatchbackResult <> taxiResult <> taxiPlusResult
        Just poolVariant -> getResult poolVariant True
      where
        getResult var cond = [NearestGoHomeDriversResult (cast personId) mbDeviceToken mblang onRide (roundToIntegral dist) var dlat dlon mode | cond]

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
      let dist = realToFrac $ distanceBetweenInMeters fromLocation $ LatLong {lat = location.lat, lon = location.lon}
      if Mb.isNothing variant || Just vehicle.variant == variant
        || ( case variant of
               Just SEDAN ->
                 info.canDowngradeToSedan
                   && vehicle.variant == SUV
               Just HATCHBACK ->
                 info.canDowngradeToHatchback
                   && (vehicle.variant == SUV || vehicle.variant == SEDAN)
               Just TAXI ->
                 info.canDowngradeToTaxi
                   && vehicle.variant == TAXI_PLUS
               _ -> False
           )
        then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, dist, location.lat, location.lon, vehicle.variant, info.mode)
        else Nothing
