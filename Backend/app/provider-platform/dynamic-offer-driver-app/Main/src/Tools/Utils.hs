module Tools.Utils where

import qualified Data.Map as M
import qualified Domain.Types.Booking as DB
import Domain.Types.Location
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SearchRequest as SR
import qualified Domain.Types.TransporterConfig as DTConf
import EulerHS.Language
import Kernel.External.Maps.HasCoordinates (getCoordinates)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.CacheFlow
import Kernel.Types.Distance (metersToHighPrecMeters)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Logging
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Error.Throwing (throwError)
import qualified Storage.Queries.BookingExtra as QBookingE
import qualified Storage.Queries.RideExtra as QRideE
import Tools.Constants

isDropInsideThreshold :: DB.Booking -> DTConf.TransporterConfig -> LatLong -> Bool
isDropInsideThreshold booking thresholdConfig currLoation = do
  let dropLocThreshold = metersToHighPrecMeters thresholdConfig.dropLocThreshold
      locationDiff = maybe 0 (\toLocation -> abs $ distanceBetweenInMeters (getCoordinates toLocation) currLoation) booking.toLocation
   in locationDiff <= dropLocThreshold

isValidRide :: DR.Ride -> Bool
isValidRide ride = maybe True (elem validRideTag) ride.rideTags -- TODO: How to remove hardcode string

isRiderEligibleForCabUpgrade :: SR.SearchRequest -> Bool
isRiderEligibleForCabUpgrade searchReq = maybe False (elem riderEligibleForCabUpgradeTag) searchReq.searchTags

fetchRideLocationData ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [Id DR.Ride] ->
  m (M.Map (Id DR.Ride) (Id DB.Booking), M.Map (Id DB.Booking) (Location, Maybe Location))
fetchRideLocationData rideIds = do
  rides <- QRideE.findByIds rideIds
  let bookingIds = map (.bookingId) rides
  bookings <- QBookingE.findByIds bookingIds
  let rideMap = M.fromList $ map (\r -> (r.id, r.bookingId)) rides
      bookingMap = M.fromList $ map (\b -> (b.id, (b.fromLocation, b.toLocation))) bookings
  pure (rideMap, bookingMap)

extractLocationFromMaps ::
  (MonadFlow m, Log m) =>
  Maybe (Id DR.Ride) ->
  M.Map (Id DR.Ride) (Id DB.Booking) ->
  M.Map (Id DB.Booking) (Location, Maybe Location) ->
  m (Maybe Location, Maybe Location)
extractLocationFromMaps mbRideId rideMap bookingMap = case mbRideId of
  Just rideId -> do
    let mbBookingId = M.lookup rideId rideMap
    case mbBookingId of
      Just bookingId -> do
        let mbLocations = M.lookup bookingId bookingMap
        case mbLocations of
          Just (from, to) -> pure (Just from, to)
          Nothing -> throwError $ BookingDoesNotExist bookingId.getId
      Nothing -> throwError $ RideDoesNotExist rideId.getId
  _ -> pure (Nothing, Nothing)
