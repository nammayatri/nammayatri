module Lib.JourneyLeg.Common.FRFSJourneyUtils where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified Data.HashMap.Strict as HM
import Data.List (partition)
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
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FRFSUtils
import Storage.CachedQueries.Merchant.MultiModalBus (BusData (..), BusDataWithRoutesInfo (..), FullBusData (..), utcToIST)
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error

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
  Just prefix -> prefix <> ":bus_locations"
  Nothing -> "bus_locations"

topVehicleCandidatesKeyFRFS :: Text -> Text
topVehicleCandidatesKeyFRFS journeyLegId = "journeyLegTopVehicleCandidates:" <> journeyLegId

resultKeyFRFS :: Text -> Text
resultKeyFRFS journeyLegId = "journeyLegResult:" <> journeyLegId

isYetToReachStop :: Text -> UTCTime -> FullBusData -> Bool
isYetToReachStop stopCode now bus =
  case bus.busData.eta_data of
    Just etaList ->
      case find (\eta -> eta.stopCode == stopCode) etaList of
        Just eta_data_for_boarding_stop -> eta_data_for_boarding_stop.arrivalTime > utcToIST now
        Nothing -> False
    Nothing -> False

processBusLegState ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) =>
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
  integratedBppConfig = do
    logDebug $ "movementDetected: " <> show movementDetected <> " journeyLegTrackingStatus: " <> show journeyLegTrackingStatus
    if (isOngoingJourneyLeg journeyLegTrackingStatus) && movementDetected
      then do
        let filteredBusData = case (mbUserBoardingStation, mbLegEndStation) of
              (_, Just destStation) -> filter (isYetToReachStop destStation.code now) allBusDataForRoute
              _ -> allBusDataForRoute
        case (mbCurrentLegDetails, routeCodeToUseForTrackVehicles, listToMaybe riderLastPoints) of
          (Just legDetails, Just rc, Just userPos) -> do
            riderConfig <- QRiderConfig.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
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
                    findfilteredBusData mbUserBoardingStation allBusDataForRoute
                  else findVehiclePositionFromSequence (reverse changedBuses)
              Nothing -> do
                logDebug "No current leg details available, returning empty list"
                pure []
          else do
            logDebug $ "Journey leg is not ongoing or movement is not detected, returning empty list" <> show journeyLegTrackingStatus
            if journeyLegTrackingStatus `elem` [JMStateTypes.InPlan, JMStateTypes.Arriving, JMStateTypes.AlmostArrived, JMStateTypes.Arrived]
              then do
                findfilteredBusData mbUserBoardingStation allBusDataForRoute
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
      findfilteredBusData :: (MonadFlow m) => Maybe Station -> [FullBusData] -> m [JT.VehiclePosition]
      findfilteredBusData mbBoardingStation allBusData = do
        let filteredBusData = case mbBoardingStation of
              Just boardingStation -> filter (isYetToReachStop boardingStation.code now) allBusData
              Nothing -> allBusData
        let (confirmedHighBuses, ghostBuses) = partition (\a -> a.busData.route_state == Just CQMMB.ConfirmedHigh) filteredBusData
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

getVehicleMetadata :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => [Text] -> DIBC.IntegratedBPPConfig -> m [Maybe BusDataWithRoutesInfo]
getVehicleMetadata vehicleNumbers integratedBppConfig = do
  let redisPrefix = case integratedBppConfig.providerConfig of
        DIBC.ONDC config -> config.redisPrefix
        _ -> Nothing
  CQMMB.withCrossAppRedisNew $ Hedis.hmGet (vehicleMetaKey redisPrefix) vehicleNumbers
  where
    vehicleMetaKey :: Maybe Text -> Text
    vehicleMetaKey mbRedisPrefix = case mbRedisPrefix of
      Just prefix -> prefix <> ":bus_metadata_v2"
      _ -> "bus_metadata_v2"

getBusLiveInfo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => Text -> DIBC.IntegratedBPPConfig -> m (Maybe BusDataWithRoutesInfo)
getBusLiveInfo vehicleNumber integratedBppConfig = listToMaybe . catMaybes <$> getVehicleMetadata [vehicleNumber] integratedBppConfig

getNearbyBusesFRFS :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig], HasField "ltsHedisEnv" r Redis.HedisEnv, HasShortDurationRetryCfg r c, HasKafkaProducer r) => LatLong -> DomainRiderConfig.RiderConfig -> DIBC.IntegratedBPPConfig -> m [BusDataWithRoutesInfo]
getNearbyBusesFRFS userPos' riderConfig integratedBppConfig = do
  let nearbyBusSearchRadius :: Double = fromMaybe 0.5 riderConfig.nearbyBusSearchRadius
  let redisPrefix = case integratedBppConfig.providerConfig of
        DIBC.ONDC config -> config.redisPrefix
        _ -> Nothing
  busesBS <- mapM (pure . decodeUtf8) =<< (CQMMB.withCrossAppRedisNew $ Hedis.geoSearch (nearbyBusKeyFRFS redisPrefix) (Hedis.FromLonLat userPos'.lon userPos'.lat) (Hedis.ByRadius nearbyBusSearchRadius "km"))
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
