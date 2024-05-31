module Storage.Queries.Person.GetNearestDriversCurrentlyOnRide
  ( NearestDriversResultCurrentlyOnRide (..),
    getNearestDriversCurrentlyOnRide,
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
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.Person.Internal as Int
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle.Internal as Int

data NearestDriversResultCurrentlyOnRide = NearestDriversResultCurrentlyOnRide
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    lat :: Double,
    lon :: Double,
    variant :: DV.Variant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    airConditioned :: Maybe Double,
    previousRideDropLat :: Double,
    previousRideDropLon :: Double,
    distanceToDriver :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverInfo.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates)

getNearestDriversCurrentlyOnRide ::
  (MonadFlow m, MonadTime m, MonadReader r m, LT.HasLocationService m r, CoreMetrics m, CacheFlow m r, EsqDBFlow m r) =>
  [DVST.VehicleServiceTier] ->
  [ServiceTierType] ->
  LatLong ->
  Meters ->
  Id Merchant ->
  Maybe Seconds ->
  Meters ->
  [Text] ->
  Bool ->
  Bool ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide cityServiceTiers serviceTiers fromLocLatLong radiusMeters merchantId mbDriverPositionInfoExpiry _reduceRadiusValue currentRideTripCategoryValidForForwardBatching isRental isInterCity = do
  let onRideRadius = radiusMeters
  logDebug $ "On Ride radius " <> show onRideRadius
  logDebug $ "lat long" <> show fromLocLatLong
  driverLocs <- Int.getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry fromLocLatLong onRideRadius
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show driverLocs
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) False True isRental isInterCity
  logDebug $ "GetNearestDriversCurrentlyOnRide - DInfo:- " <> show driverInfos
  vehicles <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicles
  rideCurrentlyInProgress <- filter (\ride -> show ride.tripCategory `elem` currentRideTripCategoryValidForForwardBatching) <$> QRide.getInProgressByDriverIds (map (.id) drivers)
  let rideEndLocations = mapMaybe (.toLocation) rideCurrentlyInProgress
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicle:- " <> show (length vehicles) <> " Drivers:- " <> show (length drivers) <> "Rides" <> show (length rideCurrentlyInProgress) <> "RideLocs:- " <> show (length rideEndLocations)
  let res = linkArrayListForOnRide rideCurrentlyInProgress rideEndLocations driverLocs driverInfos vehicles drivers (fromIntegral onRideRadius :: Double)
  logDebug $ "GetNearestDriversCurrentlyOnRide Result:- " <> show (length res)
  return res
  where
    linkArrayListForOnRide rideCurrentlyInProgress rideEndLocations driverLocations driverInformations vehicles persons onRideRadius =
      let locationHashMap = HashMap.fromList $ (\loc -> (loc.driverId, loc)) <$> driverLocations
          personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          rideByDriverIdHashMap = HashMap.fromList $ (\ride -> (ride.driverId, ride)) <$> rideCurrentlyInProgress
          rideToLocsHashMap = HashMap.fromList $ (\loc -> (loc.id, loc)) <$> rideEndLocations
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
       in concat $ mapMaybe (buildFullDriverListOnRide rideByDriverIdHashMap rideToLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius) vehicles

    buildFullDriverListOnRide rideByDriverIdHashMap rideToLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius vehicle = do
      let driverId' = vehicle.driverId
      location <- HashMap.lookup driverId' locationHashMap
      ride <- HashMap.lookup driverId' rideByDriverIdHashMap
      rideToLoc <- ride.toLocation
      rideToLocation <- HashMap.lookup rideToLoc.id rideToLocsHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      person <- HashMap.lookup driverId' personHashMap
      let driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
          destinationPoint = LatLong {lat = rideToLocation.lat, lon = rideToLocation.lon}
          distanceFromDriverToDestination = realToFrac $ distanceBetweenInMeters driverLocationPoint destinationPoint
          distanceFromDestinationToPickup = realToFrac $ distanceBetweenInMeters fromLocLatLong destinationPoint
          onRideRadiusValidity = (distanceFromDriverToDestination + distanceFromDestinationToPickup) < onRideRadius
      -- ideally should be there inside the vehicle.selectedServiceTiers but still to make sure we have a default service tier for the driver
      let cityServiceTiersHashMap = HashMap.fromList $ (\vst -> (vst.serviceTierType, vst)) <$> cityServiceTiers
      let mbDefaultServiceTierForDriver = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
      let availableTiersWithUsageRestriction = selectVehicleTierForDriverWithUsageRestriction False person info vehicle cityServiceTiers
      let ifUsageRestricted = any (\(_, usageRestricted) -> usageRestricted) availableTiersWithUsageRestriction
      let selectedDriverServiceTiers =
            if ifUsageRestricted
              then do
                (.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction) -- no need to check for user selection
              else do
                DL.intersect vehicle.selectedServiceTiers ((.serviceTierType) <$> (map fst $ filter (not . snd) availableTiersWithUsageRestriction))
      if onRideRadiusValidity
        then do
          if null serviceTiers
            then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap) selectedDriverServiceTiers
            else do
              Just $
                mapMaybe
                  ( \serviceTier -> do
                      if serviceTier `elem` selectedDriverServiceTiers
                        then mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier
                        else Nothing
                  )
                  serviceTiers
        else Nothing
      where
        mkDriverResult mbDefaultServiceTierForDriver person info location rideToLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          Just $
            NearestDriversResultCurrentlyOnRide
              { driverId = cast person.id,
                driverDeviceToken = person.deviceToken,
                language = person.language,
                onRide = info.onRide,
                lat = location.lat,
                lon = location.lon,
                variant = vehicle.variant,
                serviceTier = serviceTier,
                serviceTierDowngradeLevel = maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver,
                airConditioned = serviceTierInfo.airConditioned,
                previousRideDropLat = rideToLocation.lat,
                previousRideDropLon = rideToLocation.lon,
                distanceToDriver = roundToIntegral $ distanceFromDriverToDestination + distanceFromDestinationToPickup,
                distanceFromDriverToDestination = roundToIntegral distanceFromDriverToDestination,
                mode = info.mode,
                clientSdkVersion = person.clientSdkVersion,
                clientBundleVersion = person.clientBundleVersion,
                clientConfigVersion = person.clientConfigVersion,
                clientDevice = person.clientDevice,
                backendConfigVersion = person.backendConfigVersion,
                backendAppVersion = person.backendAppVersion
              }
