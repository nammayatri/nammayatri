module SharedLogic.Ride where

import qualified Beckn.ACL.Status as StatusACL
import qualified Beckn.OnDemand.Utils.Common as Common
import qualified BecknV2.OnDemand.Types as Fields
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.HashMap.Strict as HM
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Domain.Action.Beckn.OnTrack as OnTrack
import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Location as Fields
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as Fields
import qualified Domain.Types.Ride as SRide
import Domain.Types.RideStatus
import qualified Domain.Types.RiderConfig as Fields
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.Trip as DTC
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (HasField)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Fields
import Kernel.Types.Id
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.EtaRoute as CQEtaRoute
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.StopInformation as QSI
import qualified Storage.Queries.Transformers.Booking as TBooking
import Tools.Error
import qualified Tools.Maps as MapSearch
import Tools.Metrics (CoreMetrics)
import TransactionLogs.Types

data GetDriverLocResp = GetDriverLocResp
  { lat :: Double,
    lon :: Double,
    lastUpdate :: UTCTime,
    pickupEtaInMinutes :: Maybe Int, -- says pickup but can be on ride values as well
    pickupDist :: Meters
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getDriverLoc ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadCatch m,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c
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
      then CallBPP.callGetDriverLocation ride.trackingUrl `catch` (handleStaleRideOnTrackUrlError ride booking)
      else do
        withLongRetry $ CallBPP.callTrack booking ride
        trackingLoc :: OnTrack.TrackingLocation <- (Redis.runInMultiCloudRedisMaybeResult $ Redis.get (Common.mkRideTrackingRedisKey ride.id.getId)) >>= fromMaybeM (InvalidRequest "Driver location not updated")
        return $
          CallBPP.GetLocationRes
            { currPoint = MapSearch.LatLong {lat = trackingLoc.gps.lat, lon = trackingLoc.gps.lon},
              lastUpdate = trackingLoc.updatedAt
            }

  (mbEta, distRemaining) <- calculateETA ride booking res.currPoint

  return $
    GetDriverLocResp
      { lat = res.currPoint.lat,
        lon = res.currPoint.lon,
        lastUpdate = res.lastUpdate,
        pickupEtaInMinutes = mbEta,
        pickupDist = Meters $ round distRemaining
      }

handleStaleRideOnTrackUrlError ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    MonadCatch m,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c
  ) =>
  SRide.Ride ->
  DB.Booking ->
  TrackUrlError ->
  m CallBPP.GetLocationRes
handleStaleRideOnTrackUrlError ride booking trackUrlErr = do
  when (trackUrlErr == InvalidRideRequest) $
    Redis.whenWithLockRedis (staleRideSyncLockKey ride.id) 60 $
      fork ("sync ride status after TrackUrlError for rideId: " <> ride.id.getId) $ do
        merchant <- CQMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
        city <- CQMOC.findById booking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
        callOnStatus Nothing booking merchant city
  throwM trackUrlErr

staleRideSyncLockKey :: Id SRide.Ride -> Text
staleRideSyncLockKey rideId = "DriverLoc:StaleRideSync:" <> rideId.getId

callOnStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c
  ) =>
  Maybe (Text -> m ()) ->
  DB.Booking ->
  DMerchant.Merchant ->
  Context.City ->
  m ()
callOnStatus mbSetSyncKey booking merchant city = do
  becknStatusReq <- StatusACL.buildStatusReqV2 (StatusACL.DStatusReq booking merchant city)
  whenJust mbSetSyncKey $ \setSyncKey -> do
    messageId <- Utils.getMessageId becknStatusReq.statusReqContext
    setSyncKey messageId
  void $ withShortRetry $ CallBPP.callStatusV2 booking.providerUrl becknStatusReq merchant.id

etaRetryAfterFailureSeconds :: Seconds
etaRetryAfterFailureSeconds = 10

maxPlausibleSpeedInMPS :: Double
maxPlausibleSpeedInMPS = 28 -- 100 km/h

-- | The leg of the trip whose ETA the driver-location ping is currently reporting, and
-- the point that leg ends at.
data EtaLegTarget = EtaLegTarget
  { leg :: CQEtaRoute.EtaLeg,
    target :: MapSearch.LatLong
  }

isOnReturnLeg :: SRide.Ride -> DB.Booking -> Bool
isOnReturnLeg ride booking =
  isJust ride.returnStartedAt
    && isJust booking.returnTime
    && (case booking.tripCategory of Just (DTC.InterCity _ _) -> True; _ -> False)

resolveLeg :: SRide.Ride -> DB.Booking -> Maybe EtaLegTarget
resolveLeg ride booking =
  case ride.status of
    NEW ->
      Just
        EtaLegTarget
          { leg = CQEtaRoute.PickupLeg,
            target = MapSearch.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
          }
    INPROGRESS
      | isOnReturnLeg ride booking -> -- round trip case
        Just
          EtaLegTarget
            { leg = CQEtaRoute.DropLeg,
              target = MapSearch.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
            }
      | otherwise -> do
        dropLocation <- case ride.toLocation of
          Just rideDropLocation -> Just rideDropLocation
          Nothing -> TBooking.getToLocation booking.bookingDetails
        Just
          EtaLegTarget
            { leg = CQEtaRoute.DropLeg,
              target = MapSearch.LatLong {lat = dropLocation.lat, lon = dropLocation.lon}
            }
    _ -> Nothing

calculateETA ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  m (Maybe Int, Double)
calculateETA ride booking driverLocation = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
  if fromMaybe False (mbRiderConfig >>= (.enableDynamicPickupEta))
    then calculateDynamicETA ride booking driverLocation (mbRiderConfig >>= (.pickupEtaRefreshIntervalSeconds))
    else calculateFlatSpeedPickupETA ride booking driverLocation

-- Old func as is, save for the status guard hoisted to the top.
calculateFlatSpeedPickupETA ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  m (Maybe Int, Double)
calculateFlatSpeedPickupETA ride booking driverLocation = do
  let mbEstimatedSpeed = ride.pickupSpeedInMPS
  let pickupLocation = MapSearch.LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
      pickupLegTarget = EtaLegTarget {leg = CQEtaRoute.PickupLeg, target = pickupLocation}
  mbCachedRoute <- CQEtaRoute.getRouteFromCache CQEtaRoute.PickupLeg ride.id
  routePoints <- case mbCachedRoute of
    Just cachedRoutePoints -> do
      return cachedRoutePoints
    Nothing -> do
      logInfo $ "No cached route found, fetching route as fallback for rideId: " <> ride.id.getId
      fetchedRoute <- fetchLegRoute ride booking driverLocation pickupLegTarget
      case fetchedRoute of
        Just route -> do
          CQEtaRoute.cacheRoute CQEtaRoute.PickupLeg ride.id route.points
          logInfo $ "Cached pickup route (fallback fetch) for rideId: " <> ride.id.getId
          return route.points
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

-- Will Use Disha instead of OSRM.
calculateDynamicETA ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  Maybe Seconds ->
  m (Maybe Int, Double)
calculateDynamicETA ride booking driverLocation mbRefreshInterval = do
  case resolveLeg ride booking of
    Nothing -> return (Nothing, 0)
    Just legTarget -> do
      let distanceAlong pts
            | null pts = realToFrac $ getHighPrecMeters $ CD.distanceBetweenInMeters driverLocation legTarget.target
            | otherwise = calculateDistanceAlongRoute driverLocation legTarget.target pts
      mbCachedRoute <- CQEtaRoute.getRouteFromCache legTarget.leg ride.id
      mbCachedBasis <- CQEtaRoute.getEtaBasis legTarget.leg ride.id

      let targetMoved = maybe False (CQEtaRoute.isBasisTargetStale legTarget.target) mbCachedBasis
          mbBasis = if targetMoved then Nothing else mbCachedBasis
          mbUsableRoute = if targetMoved then Nothing else mbCachedRoute
      when targetMoved $ do
        CQEtaRoute.invalidateRouteCache legTarget.leg ride.id
        CQEtaRoute.invalidateEtaBasis legTarget.leg ride.id
      isThrottled <- not <$> CQEtaRoute.isRefreshDue legTarget.leg ride.id
      -- A missing route must not bypass the throttle
      let needsRoute = targetMoved || not isThrottled
      mbFetched <-
        if needsRoute
          then do
            logInfo $ "Fetching " <> show legTarget.leg <> " route for rideId: " <> ride.id.getId <> " (cachedRoute=" <> show (isJust mbUsableRoute) <> " basis=" <> show (isJust mbBasis) <> " targetMoved=" <> show targetMoved <> ")"
            fetchedRoute <- fetchLegRoute ride booking driverLocation legTarget
            let nextRefresh = case fetchedRoute of
                  Just _ -> mbRefreshInterval
                  Nothing -> if isJust mbBasis then mbRefreshInterval else Just etaRetryAfterFailureSeconds
            CQEtaRoute.markRefreshed legTarget.leg ride.id nextRefresh
            case fetchedRoute of
              Just routeInfo -> do
                unless (null routeInfo.points) $ CQEtaRoute.cacheRoute legTarget.leg ride.id routeInfo.points
                whenJust routeInfo.duration $ \duration -> do
                  now <- getCurrentTime
                  CQEtaRoute.cacheEtaBasis legTarget.leg ride.id duration now (Just legTarget.target)
              Nothing -> logWarning $ "Failed to fetch " <> show legTarget.leg <> " route, falling back for rideId: " <> ride.id.getId
            return fetchedRoute
          else return Nothing

      let freshPoints = maybe [] (.points) mbFetched
          routePoints = if null freshPoints then fromMaybe [] mbUsableRoute else freshPoints
      when (null routePoints) $
        logWarning $ "No cached route found, using straight-line distance for rideId: " <> ride.id.getId
      let distanceRemaining = distanceAlong routePoints

      mbRemainingSeconds <-
        case mbFetched >>= (.duration) of
          Just freshDuration -> return $ Just (fromIntegral freshDuration :: Double) -- fetched just now
          Nothing -> case mbBasis of
            Nothing -> do
              logInfo $ "Cannot calculate ETA: no duration measured yet for rideId: " <> ride.id.getId
              return Nothing
            Just basis -> do
              now <- getCurrentTime
              let remaining = fromIntegral basis.durationSeconds - realToFrac (diffUTCTime now basis.computedAt)
              return $ Just $ max remaining (distanceRemaining / maxPlausibleSpeedInMPS)
      case mbRemainingSeconds of
        Just remainingSeconds -> return (Just $ ceiling (remainingSeconds / 60), distanceRemaining)
        _ -> return (Nothing, distanceRemaining)

legViaPoints ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  CQEtaRoute.EtaLeg ->
  m [MapSearch.LatLong]
legViaPoints ride booking = \case
  CQEtaRoute.PickupLeg -> return $ maybe [] (\prevDropLoc -> [prevDropLoc]) ride.driversPreviousRideDropLoc
  CQEtaRoute.DropLeg
    -- On the way back the outbound stops are behind the driver
    | isOnReturnLeg ride booking -> return []
    | not (fromMaybe False ride.hasStops) || null ride.stops -> return []
    | otherwise -> do
      reachedStops <- B.runInReplica $ QSI.findAllByRideId ride.id
      let reachedStopIds = map DSI.stopId reachedStops
          pendingStops = filter (\stop -> stop.id `notElem` reachedStopIds) ride.stops
      return $ map (\stop -> MapSearch.LatLong {lat = stop.lat, lon = stop.lon}) pendingStops

fetchLegRoute ::
  ( ServiceFlow m r,
    EsqDBReplicaFlow m r,
    MonadFlow m
  ) =>
  SRide.Ride ->
  DB.Booking ->
  MapSearch.LatLong ->
  EtaLegTarget ->
  m (Maybe Maps.RouteInfo)
fetchLegRoute ride booking driverLoc legTarget = do
  viaPoints <- legViaPoints ride booking legTarget.leg
  let waypointsList = driverLoc : (viaPoints <> [legTarget.target])
      waypoints = NE.fromList waypointsList
  logInfo $ "Fetching " <> show legTarget.leg <> " route for rideId: " <> ride.id.getId <> " with waypoints: " <> show (length waypointsList) <> " points (driverLoc, " <> show (length viaPoints) <> " via points, target)"
  routeResp <- try @_ @SomeException $ case legTarget.leg of
    CQEtaRoute.PickupLeg ->
      DRoute.getPickupRoutes (booking.riderId, booking.merchantId) (Just booking.riderId.getId) $
        DRoute.GetPickupRoutesReq
          { waypoints = waypoints,
            mode = Just Maps.CAR,
            calcPoints = True,
            rideId = Just ride.id
          }
    CQEtaRoute.DropLeg ->
      DRoute.getTripRoutes (booking.riderId, booking.merchantId) (Just booking.riderId.getId) $
        MapSearch.GetRoutesReq
          { waypoints = waypoints,
            mode = Just Maps.CAR,
            calcPoints = True
          }
  case routeResp of
    Left err -> do
      logError $ "Error fetching " <> show legTarget.leg <> " route for rideId: " <> ride.id.getId <> ", error: " <> show err
      return Nothing
    Right routes -> do
      case routes of
        (firstRoute : _) -> return $ Just firstRoute
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
        p : ps -> sum $ zipWith distanceBetween points (NE.tail (p NE.:| ps))
   in sumDistances routeSegment
  where
    distanceBetween :: MapSearch.LatLong -> MapSearch.LatLong -> Double
    distanceBetween p1 p2 = realToFrac $ getHighPrecMeters $ CD.distanceBetweenInMeters p1 p2

    findNearestPoint :: MapSearch.LatLong -> [MapSearch.LatLong] -> MapSearch.LatLong
    findNearestPoint loc points = case points of
      [] -> loc
      _ -> foldl1 (\p1 p2 -> if distanceBetween loc p1 <= distanceBetween loc p2 then p1 else p2) points
