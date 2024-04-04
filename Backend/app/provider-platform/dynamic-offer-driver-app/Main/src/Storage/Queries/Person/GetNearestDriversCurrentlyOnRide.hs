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
import qualified Storage.Queries.Booking.Internal as Int
import qualified Storage.Queries.DriverInformation.Internal as Int
import qualified Storage.Queries.DriverLocation.Internal as Int
import qualified Storage.Queries.DriverQuote.Internal as Int
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person.Internal as Int
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
    destinationLat :: Double,
    destinationLon :: Double,
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
  (MonadFlow m, MonadTime m, MonadReader r m, LT.HasLocationService m r, CoreMetrics m, KvDbFlow m r) =>
  [DVST.VehicleServiceTier] ->
  [ServiceTierType] ->
  LatLong ->
  Meters ->
  Id Merchant ->
  Maybe Seconds ->
  Meters ->
  Bool ->
  Bool ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide cityServiceTiers serviceTiers fromLocLatLong radiusMeters merchantId mbDriverPositionInfoExpiry reduceRadiusValue isRental isInterCity = do
  let onRideRadius = radiusMeters - reduceRadiusValue
  driverLocs <- Int.getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry fromLocLatLong onRideRadius
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) False True isRental isInterCity
  vehicles <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicles
  driverQuote <- Int.getDriverQuote $ map ((.getId) . (.id)) drivers
  bookingInfo <- Int.getBookingInfo driverQuote
  bookingLocation <- QL.getBookingLocs (mapMaybe (\b -> (.id) <$> b.toLocation) bookingInfo)
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicle:- " <> show (length vehicles) <> " Drivers:- " <> show (length drivers) <> " Dquotes:- " <> show (length driverQuote) <> " BInfos:- " <> show (length bookingInfo) <> " BLocs:- " <> show (length bookingLocation)
  let res = linkArrayListForOnRide driverQuote bookingInfo bookingLocation driverLocs driverInfos vehicles drivers (fromIntegral onRideRadius :: Double)
  logDebug $ "GetNearestDriversCurrentlyOnRide Result:- " <> show (length res)
  return res
  where
    linkArrayListForOnRide driverQuotes bookings bookingLocs driverLocations driverInformations vehicles persons onRideRadius =
      let locationHashMap = HashMap.fromList $ (\loc -> (loc.driverId, loc)) <$> driverLocations
          personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          quotesHashMap = HashMap.fromList $ (\quote -> (quote.driverId, quote)) <$> driverQuotes
          bookingHashMap = HashMap.fromList $ (\booking -> (Id booking.quoteId, booking)) <$> bookings
          bookingLocsHashMap = HashMap.fromList $ (\loc -> (loc.id, loc)) <$> bookingLocs
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
       in concat $ mapMaybe (buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius) vehicles

    buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius vehicle = do
      let driverId' = vehicle.driverId
      location <- HashMap.lookup driverId' locationHashMap
      quote <- HashMap.lookup driverId' quotesHashMap
      booking <- HashMap.lookup quote.id bookingHashMap
      toLocation <- booking.toLocation
      bookingLocation <- HashMap.lookup toLocation.id bookingLocsHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      person <- HashMap.lookup driverId' personHashMap
      let driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
          destinationPoint = LatLong {lat = bookingLocation.lat, lon = bookingLocation.lon}
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
            then Just $ mapMaybe (mkDriverResult mbDefaultServiceTierForDriver person info location bookingLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap) selectedDriverServiceTiers
            else do
              Just $
                mapMaybe
                  ( \serviceTier -> do
                      if serviceTier `elem` selectedDriverServiceTiers
                        then mkDriverResult mbDefaultServiceTierForDriver person info location bookingLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier
                        else Nothing
                  )
                  serviceTiers
        else Nothing
      where
        mkDriverResult mbDefaultServiceTierForDriver person info location bookingLocation distanceFromDriverToDestination distanceFromDestinationToPickup cityServiceTiersHashMap serviceTier = do
          serviceTierInfo <- HashMap.lookup serviceTier cityServiceTiersHashMap
          Just $ NearestDriversResultCurrentlyOnRide (cast person.id) person.deviceToken person.language info.onRide location.lat location.lon vehicle.variant serviceTier (maybe 0 (\d -> d.priority - serviceTierInfo.priority) mbDefaultServiceTierForDriver) serviceTierInfo.airConditioned bookingLocation.lat bookingLocation.lon (roundToIntegral $ distanceFromDriverToDestination + distanceFromDestinationToPickup) (roundToIntegral distanceFromDriverToDestination) info.mode person.clientSdkVersion person.clientBundleVersion person.clientConfigVersion person.clientDevice person.backendConfigVersion person.backendAppVersion
