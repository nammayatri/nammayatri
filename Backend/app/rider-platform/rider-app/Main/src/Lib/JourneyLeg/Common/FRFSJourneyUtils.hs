module Lib.JourneyLeg.Common.FRFSJourneyUtils where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
import Data.Ord (comparing)
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderConfig as DomainRiderConfig
import Domain.Types.RouteStopMapping (RouteStopMapping)
import Domain.Types.Station
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.Types as JT
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import Storage.CachedQueries.Merchant.MultiModalBus (BusData (..), BusDataWithRoutesInfo (..), FullBusData (..), utcToIST)
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

-- Helper functions for bus tracking, adapted from Lib.JourneyModule.Base
-- These functions are suffixed with CFRFS to avoid potential name clashes if Lib.JourneyModule.Base is also imported.

defaultBusTrackingConfigFRFS :: DomainRiderConfig.BusTrackingConfig
defaultBusTrackingConfigFRFS =
  DomainRiderConfig.BusTrackingConfig
    { fairScore = 4.0,
      fairScoreDistanceInMeters = 45.0,
      goodScore = 7.0,
      goodScoreDistanceInMeters = 30.0,
      maxScore = 10.0,
      maxScoreDistanceInMeters = 15.0,
      thresholdFactor = 0.5,
      thresholdSeconds = 30.0,
      movementThresholdInMeters = 25.0
    }

nearbyBusKeyFRFS :: Maybe Text -> Text
nearbyBusKeyFRFS mbRedisPrefix = case mbRedisPrefix of
  Just prefix | prefix /= "" -> prefix <> ":bus_locations"
  _ -> "bus_locations"

topVehicleCandidatesKeyFRFS :: Text -> Text
topVehicleCandidatesKeyFRFS journeyLegId = "journeyLegTopVehicleCandidates:" <> journeyLegId

resultKeyFRFS :: Text -> Text
resultKeyFRFS journeyLegId = "journeyLegResult:" <> journeyLegId

-- For a pre-assigned/entered vehicle: checks the booking's own scheduled window, the rider's recent
-- location history (not a single ping) against that vehicle's live position, and that the vehicle is
-- currently running the trip this booking was made for (not a same-vehicle different trip later the
-- same day). The bus's live position is one snapshot in time, so it's compared against whichever rider
-- point is closest in time to that snapshot -- not the freshest rider point, which may have been
-- recorded well after the bus moved on.
checkRiderNearBusFRFS ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], Redis.HedisLTSFlowEnv r, HasShortDurationRetryCfg r c, HasKafkaProducer r, Metrics.HasBAPMetrics m r) =>
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe UTCTime ->
  [APITypes.RiderLocationReq] ->
  DomainRiderConfig.RiderConfig ->
  DIBC.IntegratedBPPConfig ->
  m (Bool, Maybe Double, Maybe Text)
checkRiderNearBusFRFS vehicleNumber mbRouteCode mbBookingTripId mbBookingStartTime riderLocationHistory riderConfig integratedBppConfig = do
  now <- getCurrentTime
  -- A booking's own scheduled start (the GTFS-scheduled departure resolved for this specific booking's
  -- trip/route/station at confirm time) must be near "now" -- otherwise the rider's other booking on
  -- the same route/vehicle, days away, could wrongly ride along on this proximity check. Deliberately a
  -- generous window (12h default): a live-tracked shuttle can legitimately run hours late, this only
  -- needs to catch "wrong day", not "running behind schedule". Checked first, before any live lookup,
  -- since an obviously-wrong-day booking never needs one.
  let isWithinBookingWindow = case mbBookingStartTime of
        Nothing -> True
        Just startTime -> abs (Time.diffUTCTime now startTime) < intToNominalDiffTime (round (fromMaybe (12 * 3600) riderConfig.boardingMaxBookingStartDriftSeconds))
  if not isWithinBookingWindow
    then pure (False, Nothing, Just "OUTSIDE_BOOKING_WINDOW")
    else do
      mbBusData <- getBusLiveInfo vehicleNumber integratedBppConfig
      case mbBusData of
        Nothing -> pure (False, Nothing, Just "NO_LIVE_BUS_DATA")
        Just busData -> do
          let busPingTime = posixSecondsToUTCTime (fromIntegral busData.timestamp)
          let pingAgeSeconds = abs (Time.diffUTCTime now busPingTime)
          -- A binary trust gate for one specific vehicle, feeding an action that marks a ticket used --
          -- deliberately tighter than busTrackingConfig.thresholdSeconds (30s), which exists for the voting
          -- system's relative ranking across many candidate buses and tolerates more slop than a moving
          -- bus's position staying meaningfully close to boardingMatchRadiusInMeters.
          let maxPingAgeSeconds = fromMaybe 10.0 riderConfig.boardingBusPingMaxAgeSeconds
          let isStale = pingAgeSeconds >= realToFrac maxPingAgeSeconds
          -- Same freshness bar applied to the rider side: picking "whichever point is closest in time
          -- to the bus ping" from the whole history can otherwise let an old-but-coincidentally-aligned
          -- rider point win over a genuinely current one that better reflects where the rider actually
          -- is right now. Filtered against server time, not the client-supplied currTime, since a
          -- client clock can be skewed.
          let recentRiderPoints = filter (\p -> abs (Time.diffUTCTime now p.currTime) < realToFrac maxPingAgeSeconds) riderLocationHistory
          if isStale
            then pure (False, Nothing, Just "NO_LIVE_BUS_DATA")
            else
              if null recentRiderPoints
                then pure (False, Nothing, Just "NO_RECENT_RIDER_LOCATION")
                else do
                  let closestPoint = minimumBy (comparing (\p -> abs (Time.diffUTCTime p.currTime busPingTime))) recentRiderPoints
                  let busLoc = LatLong busData.latitude busData.longitude
                  let distanceMeters :: Double = realToFrac (highPrecMetersToMeters (distanceBetweenInMeters closestPoint.latLong busLoc))
                  let matchRadius = fromMaybe 30.0 riderConfig.boardingMatchRadiusInMeters
                  -- Same vehicle, different trip: a shuttle can run multiple trips a day, so a rider with two
                  -- same-day bookings on the same physical bus could otherwise match the wrong one. Verify the
                  -- vehicle's *currently active* trip is the one this booking was actually made for. Fails open
                  -- (doesn't block) when either side lacks the data to compare, same as the no-data case above.
                  isSameActiveTrip <- case (mbRouteCode, mbBookingTripId) of
                    (Just routeCode, Just bookingTripId) -> do
                      let (waybillNo, tripNo) = JMU.getWaybillNoAndTripNoFromTripId bookingTripId
                      scheduleDetails <- OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBppConfig
                      -- getBusTripSchedule is already scoped to this exact waybill+trip, but don't rely on that
                      -- implicitly -- confirm the returned detail really is the requested trip before trusting
                      -- its is_active_trip flag.
                      pure $
                        any
                          ( \detail ->
                              detail.is_active_trip == Just True
                                && detail.vehicle_no == vehicleNumber
                                && detail.waybill_no == Just waybillNo
                                && detail.trip_number == Just tripNo
                          )
                          scheduleDetails
                    _ -> pure True
                  let isMatch = distanceMeters <= matchRadius && isSameActiveTrip
                  let reason
                        | isMatch = Nothing
                        | distanceMeters > matchRadius = Just "TOO_FAR"
                        | otherwise = Just "TRIP_MISMATCH"
                  pure (isMatch, Just distanceMeters, reason)

isYetToReachStop :: Text -> UTCTime -> FullBusData -> Bool
isYetToReachStop stopCode now bus =
  case bus.busData.eta_data of
    Just etaList ->
      case find (\eta -> eta.stopCode == stopCode) etaList of
        Just eta_data_for_boarding_stop -> eta_data_for_boarding_stop.arrivalTime > utcToIST now
        Nothing -> False
    Nothing -> False

filterBusesYetToReachStop :: (MonadFlow m, Metrics.HasBAPMetrics m r) => Text -> UTCTime -> Bool -> Id MerchantOperatingCity -> [FullBusData] -> m [FullBusData]
filterBusesYetToReachStop stopCode now includeNoEta merchantOpCityId allBuses = do
  let (matched, rest) = partition (isYetToReachStop stopCode now) allBuses
  let busesWithNoEta = filter (\bus -> isNothing bus.busData.eta_data) rest
  unless (null busesWithNoEta) $ do
    logError $
      "Buses with no eta_data found - stopCode: " <> stopCode
        <> ", vehicles: "
        <> show (map (\bus -> (bus.vehicleNumber, bus.busData.route_id)) busesWithNoEta)
    Metrics.incrementVehicleNoEtaCounter merchantOpCityId.getId merchantOpCityId.getId "riderLocation"
  if includeNoEta
    then pure $ matched <> busesWithNoEta
    else pure matched

processBusLegState ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], Redis.HedisLTSFlowEnv r, HasShortDurationRetryCfg r c, HasKafkaProducer r, Metrics.HasBAPMetrics m r) =>
  UTCTime ->
  Maybe DJourneyLeg.JourneyLeg ->
  Maybe Text ->
  [APITypes.RiderLocationReq] ->
  Id MerchantOperatingCity ->
  Maybe Station ->
  Maybe Station ->
  [FullBusData] ->
  HM.HashMap Text (HM.HashMap Text RouteStopMapping) ->
  JMStateTypes.TrackingStatus ->
  Bool ->
  DIBC.IntegratedBPPConfig ->
  Maybe Text ->
  m [JT.VehiclePosition]
processBusLegState
  now
  mbCurrentLegDetails
  routeCodeToUseForTrackVehicles
  riderLastPoints
  merchantOperatingCityId
  mbUserBoardingStation
  mbLegEndStation
  allBusDataForRoute
  routeStopMappings
  journeyLegTrackingStatus
  movementDetected
  integratedBppConfig
  mbBookedVehicleNumber = do
    logDebug $ "movementDetected: " <> show movementDetected <> " journeyLegTrackingStatus: " <> show journeyLegTrackingStatus
    riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
    let includeNullUpcomingStops = fromMaybe False riderConfig.includeVehiclesWithNoEta
    if (isOngoingJourneyLeg journeyLegTrackingStatus) && movementDetected
      then do
        filteredBusData <- case (mbUserBoardingStation, mbLegEndStation) of
          (_, Just destStation) -> filterBusesYetToReachStop destStation.code now includeNullUpcomingStops merchantOperatingCityId allBusDataForRoute
          _ -> pure allBusDataForRoute
        case (mbCurrentLegDetails, routeCodeToUseForTrackVehicles, listToMaybe riderLastPoints) of
          (Just legDetails, Just rc, Just userPos) -> do
            let busTrackingConfig = fromMaybe defaultBusTrackingConfigFRFS riderConfig.busTrackingConfig
            nearbyBusesETA <- getNearbyBusesFRFS userPos.latLong riderConfig integratedBppConfig
            logDebug $ "nearbyBusesETA: " <> show nearbyBusesETA <> "for route_id: " <> show rc
            let scoresForBuses = scoreBusesByDistanceFRFS userPos busTrackingConfig nearbyBusesETA
            logDebug $ "scoresForBuses: " <> show scoresForBuses
            votingSystemFRFS scoresForBuses legDetails busTrackingConfig

            topCandidatesRaw <- Hedis.zRangeWithScores (topVehicleCandidatesKeyFRFS (legDetails.id.getId)) 0 (-1)
            logDebug $ "topCandidatesRaw: " <> show topCandidatesRaw
            let mbTopCandidateId = listToMaybe [TE.decodeUtf8 bs | (bs, _) <- topCandidatesRaw]
            logDebug $ "mbTopCandidateId: " <> show mbTopCandidateId
            case mbTopCandidateId of
              Just topCandVehId -> do
                let mbBestFullBusData = find (\bd -> bd.vehicleNumber == topCandVehId) filteredBusData
                logDebug $ "mbBestFullBusData: " <> show mbBestFullBusData
                case mbBestFullBusData of
                  Just bestFullBusData -> do
                    let bestBusData = bestFullBusData.busData
                    let routeStopMapping = HM.lookup bestBusData.route_id routeStopMappings
                    let upcomingStops =
                          if journeyLegTrackingStatus `elem` [JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived]
                            then getUpcomingStopsForBus routeStopMapping now mbUserBoardingStation bestBusData False -- Stops up to boarding for OnTheWay
                            else getUpcomingStopsForBus routeStopMapping now mbLegEndStation bestBusData True -- Stops to destination for Ongoing/Finishing/Completed
                    pure
                      [ JT.VehiclePosition
                          { position = Just $ LatLong bestBusData.latitude bestBusData.longitude,
                            vehicleId = topCandVehId,
                            route_state = bestBusData.route_state,
                            upcomingStops = upcomingStops
                          }
                      ]
                  Nothing -> do
                    logDebug "No best bus data available, returning empty list"
                    pure []
              Nothing -> do
                logDebug "No top candidate vehicle ID available, returning empty list"
                pure []
          _ -> do
            logDebug "No top candidate vehicle ID available, returning empty list"
            pure []
      else do
        if isOngoingJourneyLeg journeyLegTrackingStatus && not movementDetected
          then do
            logDebug $ "No current leg details available" <> show journeyLegTrackingStatus
            case mbCurrentLegDetails of
              Just legDetails -> do
                let changedBuses = fromMaybe [] legDetails.changedBusesInSequence
                logDebug $ "changedBuses: " <> show changedBuses
                if null changedBuses
                  then do
                    findfilteredBusData includeNullUpcomingStops mbUserBoardingStation allBusDataForRoute mbBookedVehicleNumber
                  else findVehiclePositionFromSequence (reverse changedBuses)
              Nothing -> do
                logDebug "No current leg details available, returning empty list"
                pure []
          else do
            logDebug $ "Journey leg is not ongoing or movement is not detected, returning empty list" <> show journeyLegTrackingStatus
            if journeyLegTrackingStatus `elem` [JMStateTypes.InPlan, JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived]
              then do
                findfilteredBusData includeNullUpcomingStops mbUserBoardingStation allBusDataForRoute mbBookedVehicleNumber
              else do
                logDebug "No filtered bus data available, returning empty list"
                pure []
    where
      findVehiclePositionFromSequence :: (MonadFlow m) => [Text] -> m [JT.VehiclePosition]
      findVehiclePositionFromSequence [] = pure []
      findVehiclePositionFromSequence (busNum : rest) = do
        logDebug $ "Looking for bus number: " <> show busNum
        case find (\bd -> bd.vehicleNumber == busNum) allBusDataForRoute of
          Just bestBusData -> do
            let routeStopMapping = HM.lookup bestBusData.busData.route_id routeStopMappings
            let upcomingStops = getUpcomingStopsForBus routeStopMapping now mbLegEndStation bestBusData.busData True
            logDebug $ "findVehiclePositionFromSequence upcomingStops: " <> show upcomingStops <> " " <> show bestBusData.busData.latitude <> " " <> show bestBusData.busData.longitude
            pure
              [ JT.VehiclePosition
                  { position = Just $ LatLong bestBusData.busData.latitude bestBusData.busData.longitude,
                    vehicleId = busNum,
                    route_state = bestBusData.busData.route_state,
                    upcomingStops = upcomingStops
                  }
              ]
          Nothing -> do
            logDebug $ "No bus data found for vehicle number: " <> show rest
            findVehiclePositionFromSequence rest
      findfilteredBusData :: (MonadFlow m, Metrics.HasBAPMetrics m r) => Bool -> Maybe Station -> [FullBusData] -> Maybe Text -> m [JT.VehiclePosition]
      findfilteredBusData includeNoEta mbBoardingStation allBusData mbVehicleNumber = do
        filteredBusData <- case mbBoardingStation of
          Just boardingStation -> filterBusesYetToReachStop boardingStation.code now includeNoEta merchantOperatingCityId allBusData
          Nothing -> pure allBusData
        let vehicleFilteredBusData = case mbVehicleNumber of
              Just vehicleNum -> filter (\bd -> bd.vehicleNumber == vehicleNum) filteredBusData
              Nothing -> filteredBusData
        let (confirmedHighBuses, ghostBuses) = partition (\a -> a.busData.route_state == Just CQMMB.ConfirmedHigh) vehicleFilteredBusData
        logInfo $ "confirmedHighBuses: " <> show (length confirmedHighBuses) <> " ghostBuses: " <> show (length ghostBuses)
        pure $
          map
            ( \bd -> do
                let routeStopMapping = HM.lookup bd.busData.route_id routeStopMappings
                JT.VehiclePosition
                  { position = Just $ LatLong bd.busData.latitude bd.busData.longitude,
                    vehicleId = bd.vehicleNumber,
                    route_state = bd.busData.route_state,
                    upcomingStops = getUpcomingStopsForBus routeStopMapping now mbBoardingStation bd.busData False
                  }
            )
            confirmedHighBuses

getUpcomingStopsForBus ::
  Maybe (HM.HashMap Text RouteStopMapping) ->
  UTCTime -> -- Current time (`now`)
  Maybe Station -> -- The target station (e.g., boarding or destination)
  BusData -> -- The specific bus's data, containing `eta_data`
  Bool -> -- `True` if filtering from current time onwards, `False` otherwise (e.g., for OnTheWay, we might want all stops up to boarding)
  [JT.NextStopDetails]
getUpcomingStopsForBus mbRouteStopMapping now mbTargetStation busData filterFromCurrentTime =
  case (busData.eta_data, mbRouteStopMapping) of
    (Just etaData, Just routeStopMapping) ->
      let -- Filter stops up to the target station
          stopsUpToTarget :: [CQMMB.BusStopETA] = case mbTargetStation of
            Just targetStation -> fst $ foldl' (\(eta_data_acc, foundTarget) bs -> if not foundTarget then (bs : eta_data_acc, bs.stopCode == targetStation.code) else (eta_data_acc, True)) ([], False) etaData
            Nothing -> etaData

          -- Further filter from current time if required
          filteredStops =
            if filterFromCurrentTime
              then filter (\bs -> bs.arrivalTime > utcToIST now) stopsUpToTarget
              else stopsUpToTarget

          -- Map BusStopETA to NextStopDetails
          toNextStopDetails bs =
            let mbStop = HM.lookup bs.stopCode routeStopMapping
             in case mbStop of
                  Just stop -> do
                    JT.NextStopDetails
                      { stopCode = bs.stopCode,
                        sequenceNumber = stop.sequenceNum,
                        travelTime = Just . Seconds $ div ((.getSeconds) . nominalDiffTimeToSeconds $ diffUTCTime bs.arrivalTime (utcToIST now)) 60,
                        travelDistance = Nothing,
                        stopName = Just stop.stopName
                      }
                  Nothing ->
                    JT.NextStopDetails
                      { stopCode = bs.stopCode,
                        sequenceNumber = 0, -- THIS CASE SHOULD NEVER COME, IF ITS HAPPENEING SOMETHING IS OFF IN DATA
                        travelTime = Nothing,
                        travelDistance = Nothing,
                        stopName = Nothing
                      }
       in map toNextStopDetails filteredStops
    _ -> []

getVehicleMetadata :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], Redis.HedisLTSFlowEnv r, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [Text] -> DIBC.IntegratedBPPConfig -> m [Maybe BusDataWithRoutesInfo]
getVehicleMetadata vehicleNumbers integratedBppConfig = do
  let redisPrefix = case integratedBppConfig.providerConfig of
        DIBC.ONDC config -> config.redisPrefix
        DIBC.DIRECT config -> config.redisPrefix
        _ -> Nothing
  Hedis.runInMultiCloudLTSRedisForMaybeListFromReplica $ Hedis.hmGet (vehicleMetaKey redisPrefix) vehicleNumbers
  where
    vehicleMetaKey :: Maybe Text -> Text
    vehicleMetaKey mbRedisPrefix = case mbRedisPrefix of
      Just prefix | prefix /= "" -> prefix <> ":bus_metadata_v2"
      _ -> "bus_metadata_v2"

getBusLiveInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], Redis.HedisLTSFlowEnv r, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Text -> DIBC.IntegratedBPPConfig -> m (Maybe BusDataWithRoutesInfo)
getBusLiveInfo vehicleNumber integratedBppConfig = listToMaybe . catMaybes <$> getVehicleMetadata [vehicleNumber] integratedBppConfig

getNearbyBusesFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], Redis.HedisLTSFlowEnv r, HasShortDurationRetryCfg r c, HasKafkaProducer r) => LatLong -> DomainRiderConfig.RiderConfig -> DIBC.IntegratedBPPConfig -> m [BusDataWithRoutesInfo]
getNearbyBusesFRFS userPos' riderConfig integratedBppConfig = do
  let nearbyBusSearchRadius :: Double = fromMaybe 0.5 riderConfig.nearbyBusSearchRadius
  let redisPrefix = case integratedBppConfig.providerConfig of
        DIBC.ONDC config -> config.redisPrefix
        DIBC.DIRECT config -> config.redisPrefix
        _ -> Nothing
  busesBS <-
    mapM (pure . decodeUtf8) =<< Hedis.runInMultiCloudLTSRedisForListFromReplica (Hedis.geoSearch (nearbyBusKeyFRFS redisPrefix) (Hedis.FromLonLat userPos'.lon userPos'.lat) (Hedis.ByRadius nearbyBusSearchRadius "km"))
  logDebug $ "getNearbyBusesFRFS: busesBS: " <> show busesBS
  buses <-
    if null busesBS
      then do
        logDebug $ "getNearbyBusesFRFS: No buses found in geo search, returning empty list"
        pure []
      else do
        logDebug $ "getNearbyBusesFRFS: Fetching bus metadata for " <> show (length busesBS) <> " buses"
        getVehicleMetadata busesBS integratedBppConfig
  logDebug $ "getNearbyBusesFRFS: buses: " <> show buses
  pure $ catMaybes buses

scoreByDistanceFRFS :: Double -> DomainRiderConfig.BusTrackingConfig -> Double
scoreByDistanceFRFS distance busTrackingConfig
  | distance <= busTrackingConfig.maxScoreDistanceInMeters = busTrackingConfig.maxScore
  | distance <= busTrackingConfig.goodScoreDistanceInMeters = busTrackingConfig.goodScore
  | distance <= busTrackingConfig.fairScoreDistanceInMeters = busTrackingConfig.fairScore
  | otherwise = 0

scoreBusesByDistanceFRFS :: APITypes.RiderLocationReq -> DomainRiderConfig.BusTrackingConfig -> [BusDataWithRoutesInfo] -> [(BusDataWithRoutesInfo, Double)]
scoreBusesByDistanceFRFS passengerLoc busTrackingConfig = map assignScore . filter isRecent
  where
    now = passengerLoc.currTime

    isRecent :: BusDataWithRoutesInfo -> Bool
    isRecent bus =
      let pingTime = posixSecondsToUTCTime (fromIntegral bus.timestamp)
          timeDiff = abs (Time.diffUTCTime now pingTime)
       in timeDiff < (realToFrac busTrackingConfig.thresholdSeconds)

    assignScore :: BusDataWithRoutesInfo -> (BusDataWithRoutesInfo, Double)
    assignScore bus =
      let busLoc = LatLong bus.latitude bus.longitude
          distanceMeters = distanceBetweenInMeters passengerLoc.latLong busLoc
          dist :: Double = realToFrac (highPrecMetersToMeters distanceMeters) -- Ensure conversion to simple meters
          score = scoreByDistanceFRFS dist busTrackingConfig
       in (bus, score)

isWorseThanThresholdFRFS :: Double -> Double -> Double -> Bool
isWorseThanThresholdFRFS candidateScore bestScore worseThreshold = candidateScore < (worseThreshold * bestScore)

addAllScoresFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithRoutesInfo, Double)] -> DJourneyLeg.JourneyLeg -> m ()
addAllScoresFRFS scoredBuses leg = do
  forM_ scoredBuses $ \(bus, points) -> do
    whenJust bus.vehicle_number $ \vehicle ->
      Hedis.zIncrBy (topVehicleCandidatesKeyFRFS leg.id.getId) (round points) vehicle

removeWorstMembersFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [Text] -> [(Text, Double)] -> DJourneyLeg.JourneyLeg -> Double -> DomainRiderConfig.BusTrackingConfig -> m ()
removeWorstMembersFRFS currentResultMembers allCandidates leg bestScore busTrackingConfig = do
  let membersToRemove =
        filter
          ( \m ->
              let scoreM = lookup m allCandidates
               in maybe False (\score -> isWorseThanThresholdFRFS score bestScore busTrackingConfig.thresholdFactor) scoreM
          )
          currentResultMembers
  forM_ membersToRemove $ \m -> Redis.srem (resultKeyFRFS leg.id.getId) [m]

addBetterMembersFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(Text, Double)] -> DJourneyLeg.JourneyLeg -> Double -> DomainRiderConfig.BusTrackingConfig -> m ()
addBetterMembersFRFS allCandidates leg bestScore busTrackingConfig = do
  forM_ allCandidates $ \(candidate, score) -> do
    unless (isWorseThanThresholdFRFS score bestScore busTrackingConfig.thresholdFactor) $
      Redis.sAddExp (resultKeyFRFS leg.id.getId) [candidate] 3600

votingSystemFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [(BusDataWithRoutesInfo, Double)] -> DJourneyLeg.JourneyLeg -> DomainRiderConfig.BusTrackingConfig -> m ()
votingSystemFRFS scoredBuses leg busTrackingConfig = do
  addAllScoresFRFS scoredBuses leg
  Hedis.expire (topVehicleCandidatesKeyFRFS leg.id.getId) 3600
  bestCandidateResult <- Hedis.zrevrangeWithscores (topVehicleCandidatesKeyFRFS leg.id.getId) 0 0
  case bestCandidateResult of
    [] -> pure ()
    ((bestVehicleNumber, bestScore) : _) -> do
      let busesChanged = leg.changedBusesInSequence
      case busesChanged of
        Nothing -> QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just [bestVehicleNumber]}
        Just changedBusesInSequence ->
          case safeTail changedBusesInSequence of
            Nothing -> QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just [bestVehicleNumber]}
            Just x ->
              if x == bestVehicleNumber
                then pure ()
                else QJourneyLeg.updateByPrimaryKey leg {DJourneyLeg.changedBusesInSequence = Just $ changedBusesInSequence <> [bestVehicleNumber]}
      allCandidatesRaw <- Hedis.zRangeWithScores (topVehicleCandidatesKeyFRFS leg.id.getId) 0 (-1)
      let allCandidates = [(TE.decodeUtf8 bs, score) | (bs, score) <- allCandidatesRaw]
      currentResultMembers :: [Text] <- Hedis.sMembers (resultKeyFRFS leg.id.getId)
      Hedis.expire (resultKeyFRFS leg.id.getId) 3600
      removeWorstMembersFRFS currentResultMembers allCandidates leg bestScore busTrackingConfig
      addBetterMembersFRFS allCandidates leg bestScore busTrackingConfig

isOngoingJourneyLeg :: JMStateTypes.TrackingStatus -> Bool
isOngoingJourneyLeg legStatus = legStatus `elem` [JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived, JMStateTypes.Ongoing, JMStateTypes.Finishing]
