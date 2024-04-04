module Storage.Queries.Person.GetNearestGoHomeDrivers
  ( getNearestGoHomeDrivers,
    NearestGoHomeDriversResult (..),
    NearestGoHomeDriversReq (..),
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import Domain.Types.DriverInformation as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.ServiceTierType as DVST
import Domain.Types.Vehicle as DV
import Domain.Types.VehicleServiceTier as DVST
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.VehicleServiceTier
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal as Int
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestGoHomeDriversReq = NearestGoHomeDriversReq
  { cityServiceTiers :: [DVST.VehicleServiceTier],
    serviceTiers :: [ServiceTierType],
    fromLocation :: LatLong,
    nearestRadius :: Meters,
    homeRadius :: Meters,
    merchantId :: Id Merchant,
    driverPositionInfoExpiry :: Maybe Seconds,
    isRental :: Bool,
    isInterCity :: Bool
  }

data NearestGoHomeDriversResult = NearestGoHomeDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.Variant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    airConditioned :: Maybe Double,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates)

getNearestGoHomeDrivers ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, KvDbFlow m r) =>
  NearestGoHomeDriversReq ->
  m [NearestGoHomeDriversResult]
getNearestGoHomeDrivers NearestGoHomeDriversReq {..} = do
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocation nearestRadius
  driverHomeLocs <- Int.getDriverGoHomeReqNearby (driverLocs <&> (.driverId))
  driverInfos <- Int.getDriverInfosWithCond (driverHomeLocs <&> (.driverId)) True False isRental isInterCity
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
      -- ideally should be there inside the vehicle.selectedServiceTiers but still to make sure we have a default service tier for the driver
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let mbDefaultServiceTierForDriver = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let availableTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction False person info vehicle cityServiceTiers
      let ifUsageRestricted = any (\(_, usageRestricted) -> usageRestricted) availableTiersWithUsageRestriction
      let selectedDriverServiceTiers =
            if ifUsageRestricted
              then do
                (.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction) -- no need to check for user selection always send for available tiers
              else do
                DL.intersect vehicle.selectedServiceTiers ((.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction))
      if null serviceTiers
        then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap) selectedDriverServiceTiers
        else do
          Just $
            mapMaybe
              ( \serviceTier -> do
                  if serviceTier `elem` selectedDriverServiceTiers
                    then mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier
                    else Nothing
              )
              serviceTiers
      where
        mkDriverResult mbDefaultServiceTierForDriver person vehicle info dist cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          Just $ NearestGoHomeDriversResult (cast person.id) person.deviceToken person.language info.onRide (roundToIntegral dist) vehicle.variant serviceTier (maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver) serviceTierInfo.airConditioned location.lat location.lon info.mode person.clientSdkVersion person.clientBundleVersion person.clientConfigVersion person.clientDevice person.backendConfigVersion person.backendAppVersion
