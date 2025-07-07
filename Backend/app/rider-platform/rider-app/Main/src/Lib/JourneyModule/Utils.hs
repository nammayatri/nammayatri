module Lib.JourneyModule.Utils where

import BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import qualified Data.Geohash as Geohash
import Data.List (groupBy, nub, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.RecentLocation as DTRL
import Domain.Types.Route
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Trip as DTrip
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.MultiModalBus as MultiModalBus
import qualified Storage.CachedQueries.Merchant.MultiModalSuburban as MultiModalSuburban
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RouteStopTimeTable as GRSM
import Storage.GraphqlQueries.Client (mapToServiceTierType)
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.RecentLocation as SQRL
import qualified Storage.Queries.VehicleRouteMapping as QVehicleRouteMapping
import qualified System.Environment as Se
import Tools.Maps (LatLong (..))

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input straightLineDistance distance maximumWalkDistance straightLineThreshold = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if distance > maximumWalkDistance && straightLineDistance > straightLineThreshold then DTrip.Taxi else DTrip.Walk
  MultiModal.Bus -> DTrip.Bus
  MultiModal.Unspecified -> DTrip.Taxi

mkJourneyUpdateInProgressKey :: Id Journey -> Text
mkJourneyUpdateInProgressKey journeyId = "Journey:UpdateInProgress:JourneyId-" <> journeyId.getId

journeyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m Bool
journeyUpdateInProgress journeyId = do
  fromMaybe False <$> (Hedis.withMasterRedis $ Hedis.get (mkJourneyUpdateInProgressKey journeyId))

withJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
withJourneyUpdateInProgress journeyId actions = do
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) True 120
  a <- actions
  Hedis.withMasterRedis $ Hedis.setExp (mkJourneyUpdateInProgressKey journeyId) False 120
  return a

whenJourneyUpdateInProgress ::
  CacheFlow m r =>
  Id Journey ->
  m a ->
  m a
whenJourneyUpdateInProgress journeyId actions = do
  gotLock <- journeyUpdateInProgress journeyId
  if gotLock
    then do
      threadDelayMilliSec 200
      whenJourneyUpdateInProgress journeyId actions
    else actions

data UpcomingVehicleInfo = UpcomingVehicleInfo
  { routeCode :: Text,
    serviceType :: Spec.ServiceTierType,
    arrivalTimeInSeconds :: Seconds,
    nextAvailableTimings :: (TimeOfDay, TimeOfDay),
    source :: SourceType
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Data type representing upcoming trip information
data UpcomingTripInfo = UpcomingTripInfo
  { busFrequency :: Maybe Seconds,
    vehicleFrequency :: Maybe Seconds,
    upcomingBuses :: [UpcomingVehicleInfo],
    upcomingVehicles :: [UpcomingVehicleInfo]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data LegServiceTier = LegServiceTier
  { serviceTierName :: Text,
    serviceTierType :: Spec.ServiceTierType,
    serviceTierDescription :: Text,
    via :: Maybe Text,
    trainTypeCode :: Maybe Text,
    fare :: PriceAPIEntity,
    quoteId :: Id DFRFSQuote.FRFSQuote
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Data structure to represent available routes grouped by service tier
data AvailableRoutesByTier = AvailableRoutesByTier
  { serviceTier :: Spec.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierDescription :: Maybe Text,
    via :: Maybe Text,
    trainTypeCode :: Maybe Text,
    quoteId :: Maybe (Id DFRFSQuote.FRFSQuote),
    availableRoutes :: [Text],
    nextAvailableBuses :: [Seconds],
    nextAvailableTimings :: [(TimeOfDay, TimeOfDay)],
    fare :: PriceAPIEntity,
    source :: SourceType
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

getISTArrivalTime :: TimeOfDay -> UTCTime -> UTCTime
getISTArrivalTime timeOfDay currentTime = do
  let currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
  UTCTime (utctDay currentTimeIST) (timeOfDayToTime timeOfDay)
  where
    istOffset :: Double = 5.5 * 3600

-- | Helper function to get IST offset and current time in IST timezone
getISTTimeInfo :: UTCTime -> (Double, UTCTime)
getISTTimeInfo currentTime =
  let istOffset :: Double = 5.5 * 3600
      currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
   in (istOffset, currentTimeIST)

fetchLiveBusTimings ::
  ( HasField "ltsHedisEnv" r Hedis.HedisEnv,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  [Text] ->
  Text ->
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopTimeTable]
fetchLiveBusTimings routeCodes stopCode currentTime integratedBppConfig mid mocid = do
  allRouteWithBuses <- MultiModalBus.getBusesForRoutes routeCodes
  routeStopTimes <- mapM processRoute allRouteWithBuses
  let flattenedRouteStopTimes = concat routeStopTimes
  disableLiveBuses <- fromMaybe False . (>>= readMaybe) <$> (liftIO $ Se.lookupEnv "DISABLE_LIVE_BUSES")
  logDebug $ "allRouteWithBuses: " <> show allRouteWithBuses <> " routeStopTimes: " <> show routeStopTimes <> " flattenedRouteStopTimes: " <> show flattenedRouteStopTimes <> " disableLiveBuses: " <> show disableLiveBuses
  if not (null flattenedRouteStopTimes) && not disableLiveBuses
    then return flattenedRouteStopTimes
    else measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode) "fetch route stop timing through graphql"
  where
    processRoute routeWithBuses = do
      let busEtaData = concatMap (\bus -> map (\eta -> (bus.vehicleNumber, eta)) $ fromMaybe [] (bus.busData.eta_data)) routeWithBuses.buses
          filteredBuses = filter (\(_, eta) -> eta.stopCode == stopCode) busEtaData
      logDebug $ "filteredBuses: " <> show filteredBuses <> " busEtaData: " <> show busEtaData
      vehicleRouteMappings <- forM filteredBuses $ \(vehicleNumber, etaData) -> do
        vrMapping <-
          try @_ @SomeException (OTPRest.getVehicleServiceType integratedBppConfig vehicleNumber) >>= \case
            Left _ -> return Nothing
            Right mapping -> return mapping
        case vrMapping of
          Just mapping -> return $ Just ((vehicleNumber, etaData), mapping.schedule_no)
          Nothing -> do
            mbMapping <- listToMaybe <$> QVehicleRouteMapping.findByVehicleNo vehicleNumber
            case mbMapping of
              Just mapping -> return $ Just ((vehicleNumber, etaData), mapping.typeOfService)
              Nothing -> return Nothing
      logDebug $ "vehicleRouteMappings: " <> show vehicleRouteMappings

      let validBuses = catMaybes vehicleRouteMappings
      logDebug $ "validBuses: " <> show validBuses
      let baseStopTimes = map createStopTime validBuses
      return baseStopTimes
      where
        createStopTime ((vehicleNumber, eta), mapping) = createRouteStopTimeTable routeWithBuses vehicleNumber eta mapping

    createRouteStopTimeTable routeWithBuses vehicleNumber eta mapping =
      let timeOfDay = timeToTimeOfDay $ utctDayTime eta.arrivalTime
          serviceTierType = mapToServiceTierType mapping
       in RouteStopTimeTable
            { integratedBppConfigId = integratedBppConfig.id,
              routeCode = routeWithBuses.routeId,
              stopCode = stopCode,
              timeOfArrival = timeOfDay,
              timeOfDeparture = timeOfDay, -- Using arrival time as departure time since we don't have separate departure time
              tripId = Id vehicleNumber,
              merchantId = Just mid,
              merchantOperatingCityId = Just mocid,
              createdAt = currentTime,
              updatedAt = currentTime,
              serviceTierType = serviceTierType,
              delay = Nothing,
              source = LIVE,
              stage = Nothing,
              platformCode = Nothing,
              providerStopCode = Nothing
            }

fetchLiveSubwayTimings ::
  ( HasField "ltsHedisEnv" r Hedis.HedisEnv,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  [Text] ->
  Text ->
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopTimeTable]
fetchLiveSubwayTimings routeCodes stopCode currentTime integratedBppConfig mid mocid = do
  allRouteWithTrains <- MultiModalSuburban.getTrainsForRoutes routeCodes
  let routeStopTimes = concatMap processRoute allRouteWithTrains
  if not (null routeStopTimes)
    then return routeStopTimes
    else measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode) "fetch route stop timing through graphql"
  where
    processRoute routeWithTrains =
      let filteredTrains = filter (\train -> train.stationCode == stopCode) (routeWithTrains.trains)
          baseStopTimes = map createRouteStopTimeTable filteredTrains
       in baseStopTimes ++ map (\rt -> (rt {serviceTierType = Spec.FIRST_CLASS}) :: RouteStopTimeTable) baseStopTimes

    createRouteStopTimeTable train =
      RouteStopTimeTable
        { integratedBppConfigId = integratedBppConfig.id,
          routeCode = train.trainNo,
          stopCode = stopCode,
          timeOfArrival = train.schedArrivalTime,
          timeOfDeparture = train.schedDepartureTime,
          tripId = Id train.trainNo,
          merchantId = Just mid,
          merchantOperatingCityId = Just mocid,
          createdAt = currentTime,
          updatedAt = currentTime,
          serviceTierType = Spec.SECOND_CLASS,
          delay = Just $ Seconds train.delayArrival,
          source = LIVE,
          stage = Nothing,
          platformCode = Nothing,
          providerStopCode = Nothing
        }

fetchLiveTimings ::
  ( HasField "ltsHedisEnv" r Hedis.HedisEnv,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  [Text] ->
  Text ->
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  m [RouteStopTimeTable]
fetchLiveTimings routeCodes stopCode currentTime integratedBppConfig mid mocid vc = case vc of
  Enums.SUBWAY -> fetchLiveSubwayTimings routeCodes stopCode currentTime integratedBppConfig mid mocid
  Enums.BUS -> fetchLiveBusTimings routeCodes stopCode currentTime integratedBppConfig mid mocid
  _ -> measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode) "fetch route stop timing through graphql"

-- | Find all possible routes from originStop to destinationStop with trips in the next hour
-- grouped by service tier type
findPossibleRoutes ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  Maybe [LegServiceTier] ->
  Text ->
  Text ->
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  m (Maybe Text, [AvailableRoutesByTier])
findPossibleRoutes mbAvailableServiceTiers fromStopCode toStopCode currentTime integratedBppConfig mid mocid vc = do
  -- Get route mappings that contain the origin stop
  fromRouteStopMappings <- OTPRest.getRouteStopMappingByStopCode fromStopCode integratedBppConfig

  -- Get route mappings that contain the destination stop
  toRouteStopMappings <- OTPRest.getRouteStopMappingByStopCode toStopCode integratedBppConfig

  -- Find common routes that have both the origin and destination stops
  -- and ensure that from-stop comes before to-stop in the route sequence
  let fromRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) fromRouteStopMappings
      toRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) toRouteStopMappings
      validRoutes =
        [ fromRouteCode
          | (fromRouteCode, fromSeq) <- fromRouteStopMap,
            (toRouteCode, toSeq) <- toRouteStopMap,
            fromRouteCode == toRouteCode && fromSeq < toSeq -- Ensure correct sequence
        ]

  -- Get the timing information for these routes at the origin stop

  routeStopTimings <- fetchLiveTimings validRoutes fromStopCode currentTime integratedBppConfig mid mocid vc
  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  let source = fromMaybe GTFS $ listToMaybe $ map (.source) routeStopTimings

  let sortedTimings =
        sortBy
          ( \a b ->
              let aTime = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime a.timeOfArrival currentTime) currentTimeIST
                  bTime = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime b.timeOfArrival currentTime) currentTimeIST
               in compare aTime bTime
          )
          routeStopTimings

  -- Group by service tier
  let groupedByTier = groupBy (\a b -> a.serviceTierType == b.serviceTierType) $ sortBy (comparing (.serviceTierType)) routeStopTimings
  logDebug $ "groupedByTier: " <> show groupedByTier <> " sortedTimings: " <> show sortedTimings <> " routeStopTimings: " <> show routeStopTimings
  -- For each service tier, collect route information
  results <- forM groupedByTier $ \timingsForTier -> do
    let serviceTierType = if null timingsForTier then Spec.ORDINARY else (head timingsForTier).serviceTierType
        routeCodesForTier = nub $ map (.routeCode) timingsForTier

    -- Get route details to include the short name
    routeDetails <-
      mapM
        ( \routeCode -> OTPRest.getRouteByRouteId integratedBppConfig routeCode
        )
        routeCodesForTier
    let validRouteDetails = catMaybes routeDetails
        routeShortNames = nub $ map (.shortName) validRouteDetails

    -- Calculate arrival times in seconds
    let arrivalTimes =
          [ estimatedArrivalTimeInSeconds
            | timing <- timingsForTier,
              let arrivalTimeInSeconds' = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime timing.timeOfArrival currentTime) currentTimeIST,
              let secondsValue = fromIntegral (getSeconds arrivalTimeInSeconds') :: Double,
              let estimatedArrivalTimeInSeconds
                    | arrivalTimeInSeconds' > 0 = arrivalTimeInSeconds'
                    | abs secondsValue > 86400 = Seconds $ round $ (7 * 86400) + secondsValue
                    | otherwise = Seconds $ round $ 86400 + secondsValue
          ]

    let nextAvailableTimings = timingsForTier <&> (\timing -> (timing.timeOfArrival, timing.timeOfDeparture))

    (mbFare, serviceTierName, serviceTierDescription, quoteId, via, trainTypeCode) <- do
      case mbAvailableServiceTiers of
        Just availableServiceTiers -> do
          let availableServiceTier = find (\tier -> tier.serviceTierType == serviceTierType) availableServiceTiers
              quoteId = availableServiceTier <&> (.quoteId)
              serviceTierName = availableServiceTier <&> (.serviceTierName)
              serviceTierDescription = availableServiceTier <&> (.serviceTierDescription)
              via = availableServiceTier >>= (.via)
              trainTypeCode = availableServiceTier >>= (.trainTypeCode)
              mbFare = availableServiceTier <&> (.fare)
          return (mbFare, serviceTierName, serviceTierDescription, quoteId, via, trainTypeCode)
        Nothing -> return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)

    logDebug $ "mbFare: " <> show mbFare <> "serviceTierType: " <> show serviceTierType <> "routeShortNames: " <> show routeShortNames <> "quoteId: " <> show quoteId <> "via: " <> show via <> "trainTypeCode: " <> show trainTypeCode
    let fare = fromMaybe (PriceAPIEntity 0.0 INR) mbFare -- fix it later
    return $
      AvailableRoutesByTier
        { serviceTier = serviceTierType,
          availableRoutes = routeShortNames,
          nextAvailableBuses = sort arrivalTimes,
          serviceTierName = serviceTierName,
          serviceTierDescription = serviceTierDescription,
          quoteId = quoteId,
          via = via,
          trainTypeCode = trainTypeCode,
          fare = fare,
          nextAvailableTimings = sortBy (\a b -> compare (fst a) (fst b)) nextAvailableTimings,
          source = source
        }

  -- Only return service tiers that have available routes
  return $ ((listToMaybe sortedTimings) <&> (.routeCode), filter (\r -> not (null $ r.availableRoutes)) results)

-- | Find the top upcoming trips for a given route code and stop code
-- Returns arrival times in seconds for the upcoming trips along with route ID and service type
findUpcomingTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  Text ->
  Text ->
  Maybe Spec.ServiceTierType ->
  UTCTime ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  m UpcomingTripInfo
findUpcomingTrips routeCode stopCode mbServiceType currentTime mid mocid vc = do
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig mocid vc DIntegratedBPPConfig.MULTIMODAL

  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  routeStopTimings <-
    SIBC.fetchFirstIntegratedBPPConfigResult
      integratedBPPConfigs
      ( \integratedBPPConfig ->
          fetchLiveTimings [routeCode] stopCode currentTime integratedBPPConfig mid mocid vc
      )
  logDebug $ "routeStopTimings: " <> show routeStopTimings

  let filteredByService = case mbServiceType of
        Just serviceType -> filter (\rst -> rst.serviceTierType == serviceType) routeStopTimings
        Nothing -> routeStopTimings

  -- Combine stop timings with their calendars and get arrival times
  -- Filter out trips that have already passed the stop
  logDebug $ "filteredByService before filtering on current time : " <> show filteredByService

  let tripTimingsWithCalendars =
        [ UpcomingVehicleInfo
            { routeCode = rst.routeCode,
              serviceType = rst.serviceTierType,
              arrivalTimeInSeconds = estimatedArrivalTimeInSeconds,
              nextAvailableTimings = (rst.timeOfArrival, rst.timeOfDeparture),
              source = rst.source
            }
          | rst <- filteredByService,
            let arrivalTimeInSeconds' = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime rst.timeOfArrival currentTime) currentTimeIST,
            let secondsValue = fromIntegral (getSeconds arrivalTimeInSeconds') :: Double,
            let estimatedArrivalTimeInSeconds
                  | arrivalTimeInSeconds' > 0 = arrivalTimeInSeconds'
                  | abs secondsValue > 86400 = Seconds $ round $ (7 * 86400) + secondsValue
                  | otherwise = Seconds $ round $ 86400 + secondsValue
        ]
  logDebug $ "tripTimingsWithCalendars after filtering on current time : " <> show tripTimingsWithCalendars
  let upcomingBuses = sortBy (\a b -> compare (arrivalTimeInSeconds a) (arrivalTimeInSeconds b)) tripTimingsWithCalendars

  let busFrequency =
        if length upcomingBuses >= 3
          then do
            let gaps =
                  zipWith
                    (\a b -> arrivalTimeInSeconds b - arrivalTimeInSeconds a)
                    upcomingBuses
                    (tail upcomingBuses)

            let avgGap :: Double = fromIntegral (sum gaps) / fromIntegral (length gaps)
                gapsAreEqual = all (\gap -> abs (fromIntegral gap - avgGap) <= (0.2 * avgGap)) gaps

            if gapsAreEqual
              then Just $ round avgGap
              else Nothing
          else Nothing

  let upcomingTripInfo =
        UpcomingTripInfo
          { busFrequency = busFrequency,
            vehicleFrequency = busFrequency,
            upcomingBuses = [],
            upcomingVehicles = upcomingBuses
          }
  return upcomingTripInfo

data StopDetails = StopDetails
  { stopCode :: Text,
    stopName :: Text,
    stopLat :: Double,
    stopLon :: Double,
    stopArrivalTime :: UTCTime,
    platformNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SingleModeRouteDetails = SingleModeRouteDetails
  { fromStop :: StopDetails,
    toStop :: StopDetails,
    route :: Route,
    availableRoutes :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

measureLatency :: MonadFlow m => m a -> Text -> m a
measureLatency action label = do
  startTime <- getCurrentTime
  result <- action
  endTime <- getCurrentTime
  let latency = diffUTCTime endTime startTime
  logDebug $ label <> " Latency: " <> show latency <> " seconds"
  return result

getSingleModeRouteDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  m (Maybe SingleModeRouteDetails)
getSingleModeRouteDetails mbRouteCode (Just originStopCode) (Just destinationStopCode) integratedBppConfig mid mocid vc = do
  mbFromStop <- OTPRest.getStationByGtfsIdAndStopCode originStopCode integratedBppConfig
  mbToStop <- OTPRest.getStationByGtfsIdAndStopCode destinationStopCode integratedBppConfig
  currentTime <- getCurrentTime
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  case (mbFromStop, mbToStop) of
    (Just fromStop, Just toStop) -> do
      case (fromStop.lat, fromStop.lon, toStop.lat, toStop.lon) of
        (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
          (nextAvailableRouteCode, possibleRoutes) <- measureLatency (findPossibleRoutes Nothing originStopCode destinationStopCode currentTime integratedBppConfig mid mocid vc) "findPossibleRoutes"

          let routeCode = mbRouteCode <|> nextAvailableRouteCode
          mbRoute <-
            maybe
              (return Nothing)
              (\rc -> OTPRest.getRouteByRouteId integratedBppConfig rc)
              routeCode
          case mbRoute of
            Just route -> do
              -- Get timing information for this route at the origin stop
              originStopTimings <- fetchLiveTimings [route.code] originStopCode currentTime integratedBppConfig mid mocid vc
              destStopTimings <- fetchLiveTimings [route.code] destinationStopCode currentTime integratedBppConfig mid mocid vc

              let mbEarliestOriginTiming =
                    findEarliestTiming currentTimeIST currentTime $
                      sortBy
                        ( \a b ->
                            compare
                              (getISTArrivalTime a.timeOfDeparture currentTime)
                              (getISTArrivalTime b.timeOfDeparture currentTime)
                        )
                        originStopTimings

              -- Find the corresponding destination arrival for the same trip
              let mbDestinationTiming = do
                    originTiming <- mbEarliestOriginTiming
                    find (\dt -> dt.tripId == originTiming.tripId) destStopTimings
                  mbFirstOriginTiming = listToMaybe originStopTimings
                  mbFirstDestinationTiming = do
                    originTiming <- mbFirstOriginTiming
                    find (\dt -> dt.tripId == originTiming.tripId) destStopTimings

              let mbDepartureTime = getISTArrivalTime . (.timeOfDeparture) <$> mbEarliestOriginTiming <*> pure currentTime
                  mbArrivalTime = getISTArrivalTime . (.timeOfArrival) <$> mbDestinationTiming <*> pure currentTime
                  mbOriginPlatformCode = ((.platformCode) =<< mbEarliestOriginTiming) <|> ((.platformCode) =<< mbFirstOriginTiming) -- (.platformCode) =<< mbEarliestOriginTiming
                  mbDestinationPlatformCode = ((.platformCode) =<< mbDestinationTiming) <|> ((.platformCode) =<< mbFirstDestinationTiming)
                  fromStopDetails = StopDetails fromStop.code fromStop.name fromStopLat fromStopLon (fromMaybe currentTime mbDepartureTime) mbOriginPlatformCode
                  toStopDetails = StopDetails toStop.code toStop.name toStopLat toStopLon (fromMaybe currentTime mbArrivalTime) mbDestinationPlatformCode
              logDebug $ "fromStopDetails: " <> show fromStopDetails <> " toStopDetails: " <> show toStopDetails <> " route: " <> show route <> " possibleRoutes: " <> show possibleRoutes <> " mbEarliestOriginTiming: " <> show mbEarliestOriginTiming <> " mbDestinationTiming: " <> show mbDestinationTiming
              return $ Just $ SingleModeRouteDetails fromStopDetails toStopDetails route (concatMap (.availableRoutes) possibleRoutes)
            Nothing -> return Nothing
        _ -> return Nothing
    _ -> return Nothing
getSingleModeRouteDetails _ _ _ _ _ _ _ = return Nothing

findEarliestTiming :: UTCTime -> UTCTime -> [RouteStopTimeTable] -> Maybe RouteStopTimeTable
findEarliestTiming currentTimeIST currentTime routeStopTimings = filter (\rst -> getISTArrivalTime rst.timeOfDeparture currentTime >= currentTimeIST) routeStopTimings & listToMaybe

convertSortingType :: DMP.JourneyOptionsSortingType -> MultiModal.SortingType
convertSortingType sortType = case sortType of
  DMP.FASTEST -> MultiModal.Fastest
  DMP.MINIMUM_TRANSITS -> MultiModal.MinimumTransits
  DMP.MOST_RELEVANT -> MultiModal.MostRelevant
  _ -> MultiModal.Fastest -- Default case for any other values

-- Helper functions for recent location
convertModeToEntityType :: DTrip.MultimodalTravelMode -> DTRL.EntityType
convertModeToEntityType DTrip.Bus = DTRL.BUS
convertModeToEntityType DTrip.Metro = DTRL.METRO
convertModeToEntityType DTrip.Subway = DTRL.SUBWAY
convertModeToEntityType _ = DTRL.TAXI -- This case will never be hit due to conditional check

-- Main function to create recent location table entry
createRecentLocationForMultimodal :: (MonadFlow m, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => Journey -> m ()
createRecentLocationForMultimodal journey = do
  journeyLegs <- QJourneyLeg.findAllByJourneyId journey.id
  let onlyPublicTransportLegs = filter (\leg -> leg.mode `elem` [DTrip.Bus, DTrip.Metro, DTrip.Subway] && not (fromMaybe False leg.isDeleted)) journeyLegs
  let fare = foldl' (\acc leg -> acc + fromMaybe 0 leg.estimatedMinFare) 0 onlyPublicTransportLegs
  now <- getCurrentTime
  let legs = sortBy (comparing (.sequenceNumber)) onlyPublicTransportLegs
  let mbFirstLeg = listToMaybe legs
  let mbLastLeg = listToMaybe (reverse legs)
  let mbFirstStopCode = mbFirstLeg >>= (.fromStopDetails) >>= (.stopCode)
  let mbLastStopCode = mbLastLeg >>= (.toStopDetails) >>= (.stopCode)
  let mbRouteCode = mbFirstLeg <&> (.routeDetails) >>= listToMaybe >>= (.gtfsId)
  let mbEndLocation = mbLastLeg <&> (.endLocation)
  case (mbFirstLeg, mbFirstStopCode, mbLastStopCode, mbEndLocation) of
    (Just firstLeg, Just firstStopCode, Just lastStopCode, Just endLocation) -> do
      uuid <- generateGUID
      cityId <- journey.merchantOperatingCityId & fromMaybeM (InternalError $ "Merchant operating city id not found for journey: " <> journey.id.getId)
      let fromGeohash = T.pack <$> Geohash.encode 6 (firstLeg.startLocation.latitude, firstLeg.startLocation.longitude)
      let toGeohash = T.pack <$> Geohash.encode 6 (endLocation.latitude, endLocation.longitude)
      mbRecentLocation <- SQRL.findByRiderIdAndGeohashAndEntityType journey.riderId toGeohash fromGeohash (if length legs > 1 then DTRL.MULTIMODAL else convertModeToEntityType firstLeg.mode)
      case mbRecentLocation of
        Just recentLocation -> SQRL.increaceFrequencyById recentLocation.id
        Nothing -> do
          let recentLocation =
                DTRL.RecentLocation
                  { id = uuid,
                    riderId = journey.riderId,
                    frequency = 1,
                    entityType = if length legs > 1 then DTRL.MULTIMODAL else convertModeToEntityType firstLeg.mode,
                    address = journey.toLocationAddress,
                    fromLatLong = Just $ LatLong firstLeg.startLocation.latitude firstLeg.startLocation.longitude,
                    routeCode = if length legs > 1 then Nothing else mbRouteCode,
                    toStopCode = Just lastStopCode,
                    fromStopCode = Just firstStopCode,
                    toLatLong = LatLong endLocation.latitude endLocation.longitude,
                    merchantOperatingCityId = cityId,
                    createdAt = now,
                    updatedAt = now,
                    fare = Just fare,
                    fromGeohash = fromGeohash,
                    toGeohash = toGeohash
                  }
          SQRL.create recentLocation
    _ -> return ()

updateCRISBookingAuthCode :: (CacheFlow m r, EsqDBFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> Maybe Text -> m Bool
updateCRISBookingAuthCode booking mbBookAuthCode =
  if booking.bookingAuthCode == mbBookAuthCode
    then return False
    else do
      void $ QFRFSTicketBooking.updateBookingAuthCodeById mbBookAuthCode booking.id
      return True
