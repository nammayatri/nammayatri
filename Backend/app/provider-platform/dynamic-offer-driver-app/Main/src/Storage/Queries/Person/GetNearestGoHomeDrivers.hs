module Storage.Queries.Person.GetNearestGoHomeDrivers
  ( getNearestGoHomeDrivers,
    NearestGoHomeDriversResult (..),
    NearestGoHomeDriversReq (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Text as T
import Domain.Types.Common
import qualified Domain.Types.Common as DriverInfo
import Domain.Types.Merchant
import Domain.Types.Person as Person
import Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant as DV
import Domain.Utils
import Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common hiding (Value)
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.VehicleServiceTier
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest.Internal as Int
import qualified Storage.Queries.DriverBankAccount as QDBA
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
    isInterCity :: Bool,
    onlinePayment :: Bool,
    isValueAddNP :: Bool,
    now :: UTCTime
  }

data NearestGoHomeDriversResult = NearestGoHomeDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    variant :: DV.VehicleVariant,
    serviceTier :: ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    isSpecialLocWarrior :: Bool,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe LatLong,
    driverTags :: A.Value,
    score :: Maybe A.Value
  }
  deriving (Generic, Show, HasCoordinates)

getNearestGoHomeDrivers ::
  (MonadFlow m, MonadTime m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, ServiceFlow m r) =>
  NearestGoHomeDriversReq ->
  m [NearestGoHomeDriversResult]
getNearestGoHomeDrivers NearestGoHomeDriversReq {..} = do
  let allowedCityServiceTiers = filter (\cvst -> cvst.serviceTierType `elem` serviceTiers) cityServiceTiers
      allowedVehicleVariant = DL.nub $ concatMap (.allowedVehicleVariant) allowedCityServiceTiers
  driverLocs <- Int.getDriverLocsWithCond merchantId driverPositionInfoExpiry fromLocation nearestRadius (Just allowedVehicleVariant)
  specialLocWarriorDriverInfos <- Int.getSpecialLocWarriorDriverInfoWithCond (driverLocs <&> (.driverId)) True False isRental isInterCity
  driverHomeLocs <- Int.getDriverGoHomeReqNearby (driverLocs <&> (.driverId))
  driverInfoWithoutSpecialLocWarrior <- Int.getDriverInfosWithCond (driverHomeLocs <&> (.driverId)) True False isRental isInterCity
  let driverInfos = specialLocWarriorDriverInfos <> driverInfoWithoutSpecialLocWarrior
  logDebug $ "MetroWarriorDebugging getNearestGoHomeDrivers" <> show specialLocWarriorDriverInfos
  vehicle <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicle
  -- driverStats <- QDriverStats.findAllByDriverIds drivers
  driverBankAccounts <-
    if onlinePayment
      then QDBA.getDrivers (driverLocs <&> (.driverId))
      else return []

  logDebug $ "GetNearestDriver - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicles:- " <> show (length vehicle) <> " Drivers:- " <> show (length drivers)
  let res = linkArrayList driverLocs driverInfos vehicle drivers driverBankAccounts
  logDebug $ "GetNearestGoHomeDrivers Result:- " <> show (length res)
  logDebug $ "MetroWarriorDebugging Result:- getNearestGoHomeDrivers -" <> show res
  return res
  where
    linkArrayList driverLocations driverInformations vehicles persons driverBankAccounts =
      let personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
          vehicleHashMap = HashMap.fromList $ (\v -> (v.driverId, v)) <$> vehicles
          driverBankAccountHashMap = HashMap.fromList $ catMaybes $ (\dba -> if dba.chargesEnabled then Just (dba.driverId, dba.accountId) else Nothing) <$> driverBankAccounts
       in -- driverStatsHashMap = HashMap.fromList $ (\stats -> (stats.driverId, stats)) <$> driverStats
          concat $ mapMaybe (buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap) driverLocations

    buildFullDriverList personHashMap vehicleHashMap driverInfoHashMap driverBankAccountHashMap location = do
      let driverId' = location.driverId
      person <- HashMap.lookup driverId' personHashMap
      vehicle <- HashMap.lookup driverId' vehicleHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      _ <- if onlinePayment then HashMap.lookup driverId' driverBankAccountHashMap else Just T.empty -- is there any better way to do this?
      -- driverStats <- HashMap.lookup driverId' driverStatsHashMap
      let dist = (realToFrac $ distanceBetweenInMeters fromLocation $ LatLong {lat = location.lat, lon = location.lon}) :: Double
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let mbDefaultServiceTierForDriver = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let availableTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction False info vehicle cityServiceTiers
      let ifUsageRestricted = any (\(_, usageRestricted) -> usageRestricted) availableTiersWithUsageRestriction
      let softBlockedTiers = fromMaybe [] info.softBlockStiers
      let removeSoftBlockedTiers = filter (\stier -> stier `notElem` softBlockedTiers)
      let selectedDriverServiceTiers =
            removeSoftBlockedTiers $
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
          Just $
            NearestGoHomeDriversResult
              { driverId = cast person.id,
                driverDeviceToken = person.deviceToken,
                language = person.language,
                onRide = info.onRide,
                isSpecialLocWarrior = info.isSpecialLocWarrior,
                distanceToDriver = roundToIntegral dist,
                variant = vehicle.variant,
                serviceTier = serviceTier,
                serviceTierDowngradeLevel = maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver,
                isAirConditioned = serviceTierInfo.isAirConditioned,
                lat = location.lat,
                lon = location.lon,
                mode = info.mode,
                clientSdkVersion = person.clientSdkVersion,
                clientBundleVersion = person.clientBundleVersion,
                clientConfigVersion = person.clientConfigVersion,
                clientDevice = person.clientDevice,
                vehicleAge = getVehicleAge vehicle.mYManufacturing now,
                backendConfigVersion = person.backendConfigVersion,
                backendAppVersion = person.backendAppVersion,
                latestScheduledBooking = info.latestScheduledBooking,
                latestScheduledPickup = info.latestScheduledPickup,
                driverTags = Yudhishthira.convertTags $ fromMaybe [] person.driverTag,
                score = Nothing
              }
