module Storage.Queries.Person.GetNearestDriversCurrentlyOnRide
  ( NearestDriversResultCurrentlyOnRide (..),
    getNearestDriversCurrentlyOnRide,
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
    destinationLat :: Double,
    destinationLon :: Double,
    distanceToDriver :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverInfo.DriverMode
  }
  deriving (Generic, Show, PrettyShow, HasCoordinates)

getNearestDriversCurrentlyOnRide ::
  (MonadFlow m, MonadTime m, MonadReader r m, LT.HasLocationService m r, CoreMetrics m) =>
  Maybe Variant ->
  LatLong ->
  Meters ->
  Id Merchant ->
  Maybe Seconds ->
  Meters ->
  m [NearestDriversResultCurrentlyOnRide]
getNearestDriversCurrentlyOnRide mbVariant fromLocLatLong radiusMeters merchantId mbDriverPositionInfoExpiry reduceRadiusValue = do
  let onRideRadius = radiusMeters - reduceRadiusValue
  driverLocs <- Int.getDriverLocsWithCond merchantId mbDriverPositionInfoExpiry fromLocLatLong onRideRadius
  driverInfos <- Int.getDriverInfosWithCond (driverLocs <&> (.driverId)) False True
  vehicles <- Int.getVehicles driverInfos
  drivers <- Int.getDrivers vehicles
  driverQuote <- Int.getDriverQuote $ map ((.getId) . (.id)) drivers
  bookingInfo <- Int.getBookingInfo driverQuote
  bookingLocation <- QL.getBookingLocs (bookingInfo <&> (.toLocation.id))
  logDebug $ "GetNearestDriversCurrentlyOnRide - DLoc:- " <> show (length driverLocs) <> " DInfo:- " <> show (length driverInfos) <> " Vehicle:- " <> show (length vehicles) <> " Drivers:- " <> show (length drivers) <> " Dquotes:- " <> show (length driverQuote) <> " BInfos:- " <> show (length bookingInfo) <> " BLocs:- " <> show (length bookingLocation)
  let res = linkArrayListForOnRide driverQuote bookingInfo bookingLocation driverLocs driverInfos vehicles drivers (fromIntegral onRideRadius :: Double)
  logDebug $ "GetNearestDriversCurrentlyOnRide Result:- " <> show (length res)
  return (makeNearestDriversResult =<< res)
  where
    linkArrayListForOnRide driverQuotes bookings bookingLocs driverLocations driverInformations vehicles persons onRideRadius =
      let locationHashMap = HashMap.fromList $ (\loc -> (loc.driverId, loc)) <$> driverLocations
          personHashMap = HashMap.fromList $ (\p -> (p.id, p)) <$> persons
          quotesHashMap = HashMap.fromList $ (\quote -> (quote.driverId, quote)) <$> driverQuotes
          bookingHashMap = HashMap.fromList $ (\booking -> (Id booking.quoteId, booking)) <$> bookings
          bookingLocsHashMap = HashMap.fromList $ (\loc -> (loc.id, loc)) <$> bookingLocs
          driverInfoHashMap = HashMap.fromList $ (\info -> (info.driverId, info)) <$> driverInformations
       in mapMaybe (buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius) vehicles

    buildFullDriverListOnRide quotesHashMap bookingHashMap bookingLocsHashMap locationHashMap driverInfoHashMap personHashMap onRideRadius vehicle = do
      let driverId' = vehicle.driverId
      location <- HashMap.lookup driverId' locationHashMap
      quote <- HashMap.lookup driverId' quotesHashMap
      booking <- HashMap.lookup quote.id bookingHashMap
      bookingLocation <- HashMap.lookup booking.toLocation.id bookingLocsHashMap
      info <- HashMap.lookup driverId' driverInfoHashMap
      person <- HashMap.lookup driverId' personHashMap
      let driverLocationPoint = LatLong {lat = location.lat, lon = location.lon}
          destinationPoint = LatLong {lat = bookingLocation.lat, lon = bookingLocation.lon}
          distanceFromDriverToDestination = realToFrac $ distanceBetweenInMeters driverLocationPoint destinationPoint
          distanceFromDestinationToPickup = realToFrac $ distanceBetweenInMeters fromLocLatLong destinationPoint
          onRideRadiusValidity = (distanceFromDriverToDestination + distanceFromDestinationToPickup) < onRideRadius
      if onRideRadiusValidity
        && ( Mb.isNothing mbVariant || Just vehicle.variant == mbVariant
               || ( case mbVariant of
                      Just SEDAN ->
                        info.canDowngradeToSedan
                          && vehicle.variant == SUV
                      Just HATCHBACK ->
                        info.canDowngradeToHatchback
                          && (vehicle.variant == SUV || vehicle.variant == SEDAN)
                      Just TAXI ->
                        info.canDowngradeToTaxi
                          && (vehicle.variant == TAXI_PLUS || vehicle.variant == SEDAN || vehicle.variant == HATCHBACK || vehicle.variant == SUV)
                      _ -> False
                  )
           )
        then Just (person.id, person.deviceToken, person.language, info.onRide, info.canDowngradeToSedan, info.canDowngradeToHatchback, info.canDowngradeToTaxi, location.lat, location.lon, vehicle.variant, bookingLocation.lat, bookingLocation.lon, distanceFromDriverToDestination + distanceFromDestinationToPickup, distanceFromDriverToDestination, info.mode)
        else Nothing

    makeNearestDriversResult :: (Id Person, Maybe FCM.FCMRecipientToken, Maybe Maps.Language, Bool, Bool, Bool, Bool, Double, Double, Variant, Double, Double, Double, Double, Maybe DriverInfo.DriverMode) -> [NearestDriversResultCurrentlyOnRide]
    makeNearestDriversResult (personId, mbDeviceToken, mblang, onRide, canDowngradeToSedan, canDowngradeToHatchback, canDowngradeToTaxi, dlat, dlon, variant, destinationEndLat, destinationEndLon, dist :: Double, distanceFromDriverToDestination :: Double, mode) =
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
        getResult var cond = [NearestDriversResultCurrentlyOnRide (cast personId) mbDeviceToken mblang onRide dlat dlon var destinationEndLat destinationEndLon (roundToIntegral dist) (roundToIntegral distanceFromDriverToDestination) mode | cond]
