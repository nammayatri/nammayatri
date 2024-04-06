module Storage.Queries.Person.GetNearestGoHomeDrivers
  ( getNearestGoHomeDrivers,
    NearestGoHomeDriversResult (..),
    NearestGoHomeDriversReq (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.Vehicle as DV
import Domain.Types.VehicleServiceTier as DVST
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal as Int
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestGoHomeDriversReq = NearestGoHomeDriversReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    serviceTier :: Maybe ServiceTierType,
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
    serviceTier :: DVST.ServiceTierType,
    airConditioned :: Maybe Double,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, HasCoordinates)

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
  return res
  where
    linkArrayList driverLocations driverInformations vehicles persons =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
       in concat $ mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap) driverLocations

    buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      let dist = (realToFrac $ distanceBetweenInMeters fromLocation $ LatLong {lat = location.lat, lon = location.lon}) :: Double
      -- ideally should be there inside the info.selectedServiceTiers but still to make sure we have a default service tier for the driver
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let defaultServiceTierForDriver = (.serviceTierType) <$> (find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers)
      let selectedDriverServiceTiers =
            case defaultServiceTierForDriver of
              Just defaultServiceTierForDriver' ->
                if defaultServiceTierForDriver' `elem` info.selectedServiceTiers
                  then info.selectedServiceTiers
                  else [defaultServiceTierForDriver'] <> info.selectedServiceTiers
              Nothing -> info.selectedServiceTiers
      case serviceTier of
        Just serviceTier' ->
          if serviceTier' `elem` selectedDriverServiceTiers
            then List.singleton <$> mkDriverResult person vehicle info dist cityServiceTiersHashMap serviceTier'
            else Nothing
        Nothing -> Just (mapMaybe (mkDriverResult person vehicle info dist cityServiceTiersHashMap) selectedDriverServiceTiers)
      where
        mkDriverResult person vehicle info dist cityServiceTiersHashMap serviceTier' = do
          serviceTierInfo <- HashMap.lookup serviceTier' cityServiceTiersHashMap
          Just $ NearestGoHomeDriversResult (cast person.id) person.deviceToken person.language info.onRide (roundToIntegral dist) vehicle.variant serviceTier' serviceTierInfo.airConditioned location.lat location.lon info.mode
