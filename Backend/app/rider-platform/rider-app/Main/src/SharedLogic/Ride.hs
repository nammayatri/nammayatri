module SharedLogic.Ride where

import qualified Beckn.OnDemand.Utils.Common as Common
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Domain.Action.Beckn.OnTrack as OnTrack
import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as SRide
import Domain.Types.RideStatus
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (HasField)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.PickupRoute as CQPickupRoute
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import TransactionLogs.Types

data GetDriverLocResp = GetDriverLocResp
  { lat :: Double,
    lon :: Double,
    lastUpdate :: UTCTime,
    pickupEtaInMinutes :: Maybe Int,
    pickupDist :: Meters
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getDriverLoc ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasLongDurationRetryCfg r c
  ) =>
  Id SRide.Ride ->
  m GetDriverLocResp
getDriverLoc rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus ("Cannot track this ride" <> Text.pack (show ride.status))
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  res <-
    if isValueAddNP && isJust ride.trackingUrl
      then CallBPP.callGetDriverLocation ride.trackingUrl
      else do
        withLongRetry $ CallBPP.callTrack booking ride
        trackingLoc :: OnTrack.TrackingLocation <- (Redis.runInMultiCloudRedis False $ Redis.get (Common.mkRideTrackingRedisKey ride.id.getId)) >>= fromMaybeM (InvalidRequest "Driver location not updated")
        return $
          CallBPP.GetLocationRes
            { currPoint = MapSearch.LatLong {lat = trackingLoc.gps.lat, lon = trackingLoc.gps.lon},
              lastUpdate = trackingLoc.updatedAt
            }

  (mbPickupEta, pickupDist) <- calculatePickupETA ride booking res.currPoint

  return $
    GetDriverLocResp
      { lat = res.currPoint.lat,
        lon = res.currPoint.lon,
        lastUpdate = res.lastUpdate,
        pickupEtaInMinutes = mbPickupEta,
        pickupDist = Meters $ round pickupDist
      }

calculatePickupETA ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  m (Maybe Int, Double)
calculatePickupETA ride booking driverLocation = do
  let mbEstimatedSpeed = ride.pickupSpeedInMPS
  let pickupLocation = MapSearch.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
  mbCachedRoute <- CQPickupRoute.getPickupRouteFromCache ride.id
  routePoints <- case mbCachedRoute of
    Just cachedRoutePoints -> do
      return cachedRoutePoints
    Nothing -> do
      logInfo $ "No cached route found, fetching route as fallback for rideId: " <> ride.id.getId
      fetchedRoute <- fetchPickupRoute ride booking driverLocation pickupLocation
      case fetchedRoute of
        Just route -> do
          CQPickupRoute.cachePickupRoute ride.id route
          logInfo $ "Cached pickup route (fallback fetch) for rideId: " <> ride.id.getId
          return route
        Nothing -> do
          logWarning $ "Failed to fetch route, will use straight-line distance for rideId: " <> ride.id.getId
          return []

  distanceToPickup <-
    if null routePoints
      then do
        logWarning $ "No cached route found, using straight-line distance for rideId: " <> ride.id.getId
        return $ realToFrac $ getHighPrecMeters $ CD.distanceBetweenInMeters driverLocation pickupLocation
      else do return $ calculateDistanceAlongRoute driverLocation pickupLocation routePoints
  case mbEstimatedSpeed of
    Nothing -> do
      logInfo $ "Cannot calculate pickup ETA: missing pickup speed for rideId: " <> ride.id.getId
      return (Nothing, distanceToPickup)
    Just estimatedSpeedInMps
      | estimatedSpeedInMps <= 0 -> do
        logInfo $ "Cannot calculate pickup ETA: invalid speed for rideId: " <> ride.id.getId
        return (Nothing, distanceToPickup)
      | ride.status == NEW ->
        return (Just $ ceiling (distanceToPickup / estimatedSpeedInMps / 60), distanceToPickup)
      | otherwise -> return (Nothing, distanceToPickup)

fetchPickupRoute ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  MapSearch.LatLong ->
  m (Maybe [MapSearch.LatLong])
fetchPickupRoute ride booking driverLoc pickupLoc = do
  let middleStops = maybe [] (\prevDropLoc -> [prevDropLoc]) ride.driversPreviousRideDropLoc
      waypointsList = driverLoc : (middleStops <> [pickupLoc])
      waypoints = NE.fromList waypointsList
      pickupRouteReq =
        DRoute.GetPickupRoutesReq
          { waypoints = waypoints,
            mode = Just Maps.CAR,
            calcPoints = True,
            rideId = Just ride.id
          }
  logInfo $ "Fetching pickup route for rideId: " <> ride.id.getId <> " with waypoints: " <> show (length waypointsList) <> " points (driverLoc, " <> show (length middleStops) <> " middle stops, pickupLoc)"
  routeResp <- try @_ @SomeException $ DRoute.getPickupRoutes (booking.riderId, booking.merchantId) (Just booking.riderId.getId) pickupRouteReq
  case routeResp of
    Left err -> do
      logError $ "Error fetching pickup route for rideId: " <> ride.id.getId <> ", error: " <> show err
      return Nothing
    Right routes -> do
      case routes of
        (firstRoute : _) -> return $ Just firstRoute.points
        [] -> do
          logWarning $ "Empty route response for rideId: " <> ride.id.getId
          return Nothing

-- Calculate distance along route points from driver to pickup
calculateDistanceAlongRoute :: MapSearch.LatLong -> MapSearch.LatLong -> [MapSearch.LatLong] -> Double
calculateDistanceAlongRoute driverLoc pickupLoc routePoints =
  let nearestToDriver = findNearestPoint driverLoc routePoints
      nearestToPickup = findNearestPoint pickupLoc routePoints
      routeSegment = case (findIndex (== nearestToDriver) routePoints, findIndex (== nearestToPickup) routePoints) of
        (Just si, Just ei)
          | si <= ei -> take (ei - si + 1) $ drop si routePoints
          | otherwise -> reverse $ take (si - ei + 1) $ drop ei routePoints
        _ -> routePoints
      sumDistances points = case points of
        [] -> 0
        [_] -> 0
        _ -> sum $ zipWith distanceBetween points (tail points)
   in sumDistances routeSegment
  where
    distanceBetween :: MapSearch.LatLong -> MapSearch.LatLong -> Double
    distanceBetween p1 p2 = realToFrac $ getHighPrecMeters $ CD.distanceBetweenInMeters p1 p2

    findNearestPoint :: MapSearch.LatLong -> [MapSearch.LatLong] -> MapSearch.LatLong
    findNearestPoint loc points = case points of
      [] -> loc
      _ -> foldl1 (\p1 p2 -> if distanceBetween loc p1 <= distanceBetween loc p2 then p1 else p2) points
