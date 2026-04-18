module Lib.JourneyModule.Utils where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified Beckn.OnDemand.Utils.Common as UCommon
import BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Geohash as Geohash
import Data.List (findIndex, groupBy, nub, partition, sort, sortBy, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Data.Time.LocalTime as LocalTime
import qualified Domain.Action.UI.Dispatcher as Dispatcher
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuote as DQuote
import qualified Domain.Types.FRFSQuoteCategory as DFRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalPreferences as DMP
import Domain.Types.Person
import qualified Domain.Types.RecentLocation as DTRL
import qualified Domain.Types.RiderConfig as RCTypes
import Domain.Types.RouteDetails
import qualified Domain.Types.RouteDetails as DRouteDetails
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Seat as Seat
import qualified Domain.Types.SeatLayout as SeatLayout
import qualified Domain.Types.Trip as DTrip
import Domain.Utils (castTravelModeToVehicleCategory, mapConcurrently)
import Environment
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import qualified ExternalBPP.Flow.Fare as Flow
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Payment.Interface.Types as KT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant.MultiModalBus as MultiModalBus
import qualified Storage.CachedQueries.Merchant.MultiModalSuburban as MultiModalSuburban
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RouteStopTimeTable as GRSM
import Storage.ConfigPilot.Config.FRFSConfig (FRFSConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.RecentLocation as SQRL
import qualified Storage.Queries.RouteDetails as QRouteDetails
import System.Environment (lookupEnv)
import Tools.Error
import Tools.Maps (LatLong (..))
import qualified Tools.Maps as Maps
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.Payment as TPayment

mapWithIndex :: (MonadFlow m) => (Int -> a -> m b) -> [a] -> m [b]
mapWithIndex f = go 0
  where
    go _ [] = return []
    go idx (x : xs') = do
      y <- f idx x
      ys <- go (idx + 1) xs'
      return (y : ys)

convertMultiModalModeToTripMode :: MultiModal.GeneralVehicleType -> Meters -> Meters -> DTrip.MultimodalTravelMode
convertMultiModalModeToTripMode input straightLineDistance maximumWalkDistance = case input of
  MultiModal.MetroRail -> DTrip.Metro
  MultiModal.Subway -> DTrip.Subway
  MultiModal.Walk -> if straightLineDistance < maximumWalkDistance then DTrip.Walk else DTrip.Taxi
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
    serviceName :: Maybe Text,
    arrivalTimeInSeconds :: Seconds,
    nextAvailableTimings :: (TimeOfDay, TimeOfDay),
    source :: SourceType,
    serviceSubTypes :: Maybe [ServiceSubType]
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
    routeCode :: Maybe Text,
    via :: Maybe Text,
    trainTypeCode :: Maybe Text,
    ticketTypeCode :: Maybe Text,
    fare :: Maybe PriceAPIEntity,
    quoteId :: Id DFRFSQuote.FRFSQuote
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegOption = JourneyLegOption
  { viaPoints :: [Text],
    routeDetails :: [JourneyLegRouteDetails],
    arrivalTimes :: [Seconds],
    availableRoutes :: [Text],
    fare :: HighPrecMoney,
    duration :: Maybe Seconds,
    distance :: Maybe Distance,
    journeyLegId :: Id DJourneyLeg.JourneyLeg,
    providerRouteId :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data JourneyLegRouteDetails = JourneyLegRouteDetails
  { fromStopCode :: Text,
    toStopCode :: Text,
    subLegOrder :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data ViaRouteDetails = ViaRouteDetails
  { viaPoints :: [(Text, Text)],
    distance :: Meters,
    routeCode :: Text,
    fare :: HighPrecMoney
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

getISTArrivalTime :: TimeOfDay -> UTCTime -> UTCTime
getISTArrivalTime timeOfDay currentTime = do
  let currentTimeIST = addUTCTime (secondsToNominalDiffTime $ round istOffset) currentTime
  UTCTime (utctDay currentTimeIST) (timeOfDayToTime timeOfDay)
  where
    istOffset :: Double = 5.5 * 3600

getUTCArrivalTime :: TimeOfDay -> UTCTime -> UTCTime
getUTCArrivalTime timeOfDay currentTime =
  let istTime = getISTArrivalTime timeOfDay currentTime
   in addUTCTime (negate $ secondsToNominalDiffTime $ round istOffset) istTime
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
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
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
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Bool ->
  m [RouteStopTimeTable]
fetchLiveBusTimings routeCodes stopCode currentTime currentTimeIST integratedBppConfig mid mocid useLiveBusData = do
  -- Fetch all service tier metadata once, shared across live and static paths
  frfsServiceTierMap <-
    M.fromList . map (\t -> (t._type, t))
      <$> CQFRFSVehicleServiceTier.findAllByMerchantOperatingCityIdAndIntegratedBPPConfigId mocid integratedBppConfig.id

  (liveRouteStopTimes, fallbackRouteCodes) <-
    if useLiveBusData
      then do
        allRouteWithBuses <- MultiModalBus.getBusesForRoutes routeCodes integratedBppConfig

        -- Enrich vehicles for all routes concurrently
        enrichedRoutes <- mapConcurrently enrichRoute allRouteWithBuses

        -- Build entries (pure) and determine fallback routes
        let liveStopTimes = concatMap (buildLiveEntries frfsServiceTierMap) enrichedRoutes
            liveRouteCodes = nub $ map (.routeCode) liveStopTimes
            routesWithoutLiveTimings = filter (`notElem` liveRouteCodes) routeCodes

        logDebug $ "allRouteWithBuses: " <> show (length allRouteWithBuses) <> " flattenedLiveRouteStopTimes count: " <> show (length liveStopTimes) <> " routes with live data: " <> show liveRouteCodes
        return (liveStopTimes, routesWithoutLiveTimings)
      else return ([], routeCodes)

  staticRouteStopTimes <-
    if null fallbackRouteCodes
      then return []
      else measureLatency (fetchStaticTimings frfsServiceTierMap fallbackRouteCodes) "fetch route stop timing through getRouteBusSchedule"

  return $ liveRouteStopTimes ++ staticRouteStopTimes
  where
    -- Filters buses at the target stop and enriches each with its service tier type
    enrichRoute routeWithBuses = do
      let filteredBuses =
            [ (bus.vehicleNumber, eta)
              | bus <- routeWithBuses.buses,
                eta <- fromMaybe [] bus.busData.eta_data,
                eta.stopCode == stopCode
            ]
      enrichedBuses <- mapM getVehicleServiceType filteredBuses
      return (routeWithBuses.routeId, catMaybes enrichedBuses)

    buildLiveEntries frfsServiceTierMap (routeId, validBuses) =
      [ createRouteStopTimeTable routeId vno eta serviceTierType frfsServiceTierName LIVE
        | (vno, eta, serviceTierType) <- validBuses,
          let frfsServiceTierName = M.lookup serviceTierType frfsServiceTierMap <&> (.shortName)
      ]

    getVehicleServiceType (vno, eta) = do
      mbVehicleMetadata <- getVehicleMetadataFromInMem [integratedBppConfig] vno
      let mbServiceTier = mbVehicleMetadata <&> (\(_, metadata) -> metadata.serviceType)
      return $ mbServiceTier <&> (vno,eta,)

    -- Fetches static schedules for all fallback routes concurrently then converts using the shared tier map
    fetchStaticTimings frfsServiceTierMap routeIds = do
      allSchedules <- mapConcurrently (\routeId -> (routeId,) <$> OTPRest.getRouteBusSchedule routeId integratedBppConfig) routeIds
      return $ concatMap (\(routeId, details) -> concatMap (convertStaticSchedule routeId frfsServiceTierMap) details) allSchedules

    convertStaticSchedule routeId frfsServiceTierMap busScheduleDetail =
      let serviceTierType = busScheduleDetail.service_tier
          frfsServiceTierName = M.lookup serviceTierType frfsServiceTierMap <&> (.shortName)
          filteredEtas = filter (\eta -> eta.stopCode == stopCode && eta.arrivalTime > currentTimeIST) busScheduleDetail.eta
       in map (\eta -> createRouteStopTimeTable routeId busScheduleDetail.vehicle_no eta serviceTierType frfsServiceTierName GTFS) filteredEtas

    createRouteStopTimeTable routeCode' vehicleNumber eta serviceTierType' serviceTierName' source' =
      let timeOfDay = timeToTimeOfDay $ utctDayTime eta.arrivalTime
       in RouteStopTimeTable
            { integratedBppConfigId = integratedBppConfig.id,
              routeCode = routeCode',
              stopCode = stopCode,
              timeOfArrival = timeOfDay,
              timeOfDeparture = timeOfDay, -- Using arrival time as departure time since we don't have separate departure time
              predictedArrivalTime = Just eta.arrivalTime,
              tripId = Id vehicleNumber,
              merchantId = Just mid,
              merchantOperatingCityId = Just mocid,
              createdAt = currentTime,
              updatedAt = currentTime,
              serviceTierType = serviceTierType',
              serviceTierName = serviceTierName',
              delay = Nothing,
              source = source',
              stage = Nothing,
              platformCode = Nothing,
              providerStopCode = Nothing,
              isStageStop = Nothing
            }

fetchLiveSubwayTimings ::
  ( HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
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
    else measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode False False) "fetch route stop timing through graphql"
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
          predictedArrivalTime = Nothing,
          tripId = Id (T.concat [train.trainNo, "_T1"]),
          merchantId = Just mid,
          merchantOperatingCityId = Just mocid,
          createdAt = currentTime,
          updatedAt = currentTime,
          serviceTierType = Spec.SECOND_CLASS,
          serviceTierName = Just "SECOND_CLASS",
          delay = Just $ Seconds train.delayArrival,
          source = LIVE,
          stage = Nothing,
          platformCode = Nothing,
          providerStopCode = Nothing,
          isStageStop = Nothing
        }

fetchLiveTimings ::
  ( HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
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
  UTCTime ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  Bool ->
  Bool ->
  m [RouteStopTimeTable]
fetchLiveTimings routeCodes stopCode currentTime currentTimeIST integratedBppConfig mid mocid vc useLiveBusData calledForSubwaySingleMode = case vc of
  -- Enums.SUBWAY -> fetchLiveSubwayTimings routeCodes stopCode currentTime integratedBppConfig mid mocid -- Removed this for now.
  Enums.BUS -> fetchLiveBusTimings routeCodes stopCode currentTime currentTimeIST integratedBppConfig mid mocid useLiveBusData
  _ -> measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode False calledForSubwaySingleMode) "fetch route stop timing through graphql"

type RouteCodeText = Text

type ServiceTypeText = Text

-- | Find all possible routes from originStop to destinationStop with trips in the next hour
-- grouped by service tier type
findPossibleRoutes ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
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
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  m (Maybe Text, [AvailableRoutesByTier], [RouteStopTimeTable])
findPossibleRoutes mbAvailableServiceTiers fromStopCode toStopCode currentTime integratedBppConfig mid mocid vc sendWithoutFare useLiveBusData calledForSubwaySingleMode enableSuburbanRoundTrip = do
  -- Get route mappings that contain the origin stop
  validRoutes <- measureLatency (getRouteCodesFromTo fromStopCode toStopCode integratedBppConfig) "getRouteCodesFromTo"
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  routeStopTimings <- measureLatency (fetchLiveTimings validRoutes fromStopCode currentTime currentTimeIST integratedBppConfig mid mocid vc useLiveBusData (vc == Enums.SUBWAY && calledForSubwaySingleMode)) ("fetchLiveTimings" <> show validRoutes <> " fromStopCode: " <> show fromStopCode <> " toStopCode: " <> show toStopCode)

  -- fetch rider config
  mbRiderConfig <- measureLatency (getConfig (RiderDimensions {merchantOperatingCityId = mocid.getId})) "getConfig RiderConfig"
  let cfgMap = maybe (toCfgMap defaultBusTierSortingConfig) toCfgMap (mbRiderConfig >>= (.busTierSortingConfig))
  maxBusTimingPerTier <- liftIO $ fromMaybe 3 . (>>= readMaybe) <$> lookupEnv "BUS_TIER_MAX_PER_TIER"

  let timingsWithETA = map (\t -> (t, getETASec t currentTime currentTimeIST)) routeStopTimings

  -- Group by (Source, ServiceTier)
  let groupedBySourceAndTier =
        groupBy (\(a, _) (b, _) -> a.source == b.source && a.serviceTierType == b.serviceTierType) $
          sortOn (\(t, _) -> (t.source, tierRank cfgMap t.serviceTierType)) timingsWithETA

  -- Sort each group by ETA and take top N
  let processedGroups = map (take maxBusTimingPerTier . sortBy (comparing snd)) groupedBySourceAndTier

  -- Flatten for final return value
  let sortedTimingsWithETA = sortBy (comparing snd) $ concat processedGroups
  let sortedTimings = map fst sortedTimingsWithETA
  let upcomingBusThresholdSec = fromMaybe (Seconds 3600) (mbRiderConfig >>= (.upcomingBusThresholdSec))
  let mbServiceTierRelationshipCfg :: Maybe [RCTypes.ServiceTierRelationshipCfg] = filter (\cfg -> cfg.vehicleType == vc) <$> ((.serviceTierRelationshipCfg) =<< mbRiderConfig)
  let tierWithEarliest =
        [ (minimumMay $ map snd group, map fst group)
          | group <- processedGroups,
            not (null group)
        ]
  let sortedTierGroups =
        sortBy
          ( \(ea1, _) (ea2, _) ->
              compare
                (withinThreshold (fmap (fromIntegral . getSeconds) ea1) upcomingBusThresholdSec)
                (withinThreshold (fmap (fromIntegral . getSeconds) ea2) upcomingBusThresholdSec)
          )
          tierWithEarliest
  let groupedByTierReordered = map snd sortedTierGroups
  let tierSummary =
        map
          ( \timingsForTier ->
              let serviceTierType = if null timingsForTier then Spec.ORDINARY else (head timingsForTier).serviceTierType
                  routeCodesForTier = nub $ map (.routeCode) timingsForTier
               in (serviceTierType, routeCodesForTier)
          )
          groupedByTierReordered
  logDebug $ "findPossibleRoutes: groupedByTierReordered summary = " <> show tierSummary

  let alternateRouteHashMap :: Maybe (M.Map Spec.ServiceTierType [Spec.ServiceTierType]) =
        case mbServiceTierRelationshipCfg of
          Nothing -> Nothing
          Just serviceTierRelationshipCfg ->
            Just $
              foldl'
                ( \acc serviceTierRelationshipCfg' ->
                    M.insert serviceTierRelationshipCfg'.serviceTierType serviceTierRelationshipCfg'.canBoardIn acc
                )
                M.empty
                serviceTierRelationshipCfg

  -- For each service tier, collect route information
  resultsNested <-
    measureLatency
      ( mapConcurrently
          (processTierGroup integratedBppConfig currentTime currentTimeIST mbAvailableServiceTiers alternateRouteHashMap enableSuburbanRoundTrip vc)
          groupedByTierReordered
      )
      "concurrentTierProcessing"

  let results = concat resultsNested

  -- Only return service tiers that have available routes
  let filteredResults = filter (\r -> not (null $ r.availableRoutes) && (r.fare /= PriceAPIEntity 0.0 INR || sendWithoutFare)) results
  return (listToMaybe sortedTimings <&> (.routeCode), filteredResults, sortedTimings)

processTierGroup ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  UTCTime ->
  UTCTime ->
  Maybe [LegServiceTier] ->
  Maybe (M.Map Spec.ServiceTierType [Spec.ServiceTierType]) ->
  Bool ->
  Enums.VehicleCategory ->
  [RouteStopTimeTable] ->
  m [AvailableRoutesByTier]
processTierGroup integratedBppConfig currentTime currentTimeIST mbAvailableServiceTiers alternateRouteHashMap enableSuburbanRoundTrip vc timingsForTier = do
  let (serviceTierType, tierSource) =
        if null timingsForTier
          then (Spec.ORDINARY, GTFS)
          else ((head timingsForTier).serviceTierType, (head timingsForTier).source)
      routeCodesForTier = nub $ map (.routeCode) timingsForTier
  logDebug $ "routeCodesForTier: " <> show routeCodesForTier <> " serviceTierType: " <> show serviceTierType <> " source: " <> show tierSource

  -- Get route details to include the short name
  validRouteDetails <- measureLatency (OTPRest.getRoutesByRouteIds integratedBppConfig routeCodesForTier) ("OTPRest.getRoutesByRouteIds for tier: " <> show serviceTierType)
  logDebug $ "validRouteDetails: " <> show validRouteDetails
  let routeShortNames = nub $ map (.shortName) validRouteDetails

  -- Calculate arrival times in seconds
  let arrivalTimes =
        [ (timing.routeCode, getETASec timing currentTime currentTimeIST)
          | timing <- timingsForTier
        ]

  let nextAvailableTimings = timingsForTier <&> (\timing -> (timing.timeOfArrival, timing.timeOfDeparture))

  -- Helper to build AvailableRoutesByTier from an optional service tier
  let buildFromTier availableServiceTier = do
        let quoteId = availableServiceTier <&> (.quoteId)
            serviceTierName = availableServiceTier <&> (.serviceTierName)
            serviceTierDescription = availableServiceTier <&> (.serviceTierDescription)
            via = availableServiceTier >>= (.via)
            trainTypeCode = availableServiceTier >>= (.trainTypeCode)
            ticketTypeCode = availableServiceTier >>= (.ticketTypeCode)
            mbFare = availableServiceTier >>= (.fare)
        logDebug $ "mbFare: " <> show mbFare <> "serviceTierType: " <> show serviceTierType <> "routeShortNames: " <> show routeShortNames <> "quoteId: " <> show quoteId <> "via: " <> show via <> "trainTypeCode: " <> show trainTypeCode <> "ticketTypeCode: " <> show ticketTypeCode
        let fare = fromMaybe (PriceAPIEntity 0.0 INR) mbFare -- fix it later
        let availableRoutesInfo =
              map
                ( \routeDetail -> do
                    let routeArrivalTimes = filter (\(rtCode, _) -> rtCode == routeDetail.code) arrivalTimes
                    AvailableRoutesInfo
                      { shortName = routeDetail.shortName,
                        longName = routeDetail.longName,
                        routeCode = routeDetail.code,
                        routeTimings = sort $ map snd routeArrivalTimes,
                        isLiveTrackingAvailable = tierSource == LIVE,
                        source = tierSource
                      }
                )
                validRouteDetails
        return $
          AvailableRoutesByTier
            { serviceTier = serviceTierType,
              alsoValidServiceTypes = alternateRouteHashMap >>= M.lookup serviceTierType,
              availableRoutes = routeShortNames, -- TODO: deprecate this
              availableRoutesInfo = availableRoutesInfo,
              nextAvailableBuses = sort $ map snd arrivalTimes,
              serviceTierName = serviceTierName,
              serviceTierDescription = serviceTierDescription,
              quoteId = quoteId,
              via = via,
              trainTypeCode = trainTypeCode,
              ticketTypeCode = ticketTypeCode,
              fare = fare,
              nextAvailableTimings = sortBy (\a b -> compare (fst a) (fst b)) nextAvailableTimings,
              source = tierSource
            }

  -- Build one or more AvailableRoutesByTier entries for this serviceTierType
  case mbAvailableServiceTiers of
    Just availableServiceTiers -> do
      let matchingTiers = filter (\tier -> tier.serviceTierType == serviceTierType) availableServiceTiers
      logDebug $ "allMatchingServiceTiersForType: " <> show serviceTierType <> " -> " <> show matchingTiers
      if enableSuburbanRoundTrip && vc == Enums.SUBWAY
        then do
          -- For SUBWAY with enableSuburbanRoundTrip, return one AvailableRoutesByTier per matching tier (or a default one if none match)
          let tiersToUse =
                if null matchingTiers
                  then [Nothing]
                  else map Just matchingTiers
          mapM buildFromTier tiersToUse
        else do
          -- For other vehicle categories, preserve existing behaviour: pick at most one tier
          let availableServiceTier = listToMaybe matchingTiers
          oneResult <- buildFromTier availableServiceTier
          return [oneResult]
    Nothing -> do
      -- No available service tiers information; fall back to a default entry
      buildFromTier Nothing >>= \r -> return [r]

withinThreshold :: Maybe Seconds -> Seconds -> Int
withinThreshold s upcomingBusThresholdSec = maybe 1 (\s' -> if s' <= upcomingBusThresholdSec then 0 else 1) s

getETASec :: RouteStopTimeTable -> UTCTime -> UTCTime -> Seconds
getETASec timing currentTime currentTimeIST =
  case predictedArrivalTime timing of
    Just exactTime -> nominalDiffTimeToSeconds $ diffUTCTime exactTime currentTimeIST
    Nothing ->
      let eta = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime (timeOfArrival timing) currentTime) currentTimeIST
          secondsValue = fromIntegral (getSeconds eta) :: Double
       in if eta > 0
            then eta
            else
              if abs secondsValue > 86400
                then Seconds $ round $ (7 * 86400) + secondsValue
                else Seconds $ round $ 86400 + secondsValue

-- | Find the top upcoming trips for a given route code and stop code
-- Returns arrival times in seconds for the upcoming trips along with route ID and service type
findUpcomingTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
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
          fetchLiveTimings [routeCode] stopCode currentTime currentTimeIST integratedBPPConfig mid mocid vc True False
      )
  logDebug $ "routeStopTimings: " <> show routeStopTimings

  let filteredByService = case mbServiceType of
        Just serviceType -> filter (\rst -> rst.serviceTierType == serviceType) routeStopTimings
        Nothing -> routeStopTimings

  -- Combine stop timings with their calendars and get arrival times
  -- Filter out trips that have already passed the stop
  logDebug $ "filteredByService before filtering on current time : " <> show filteredByService

  let tripTimingsDrafts =
        [ ( UpcomingVehicleInfo
              { routeCode = rst.routeCode,
                serviceType = rst.serviceTierType,
                serviceName = rst.serviceTierName,
                arrivalTimeInSeconds = getETASec rst currentTime currentTimeIST,
                nextAvailableTimings = (rst.timeOfArrival, rst.timeOfDeparture),
                source = rst.source,
                serviceSubTypes = Nothing
              },
            rst.tripId.getId
          )
          | rst <- filteredByService
        ]

  -- Fetch serviceSubTypes for LIVE vehicles concurrently using vehicleId
  tripTimingsWithSubTypes <- (flip mapConcurrently) tripTimingsDrafts $ \(info, vehicleId) -> do
    if info.source == LIVE
      then do
        mbVehicleMetadata <- getVehicleMetadataFromInMem integratedBPPConfigs vehicleId
        let mbServiceSubTypes = mbVehicleMetadata >>= (\(_, metadata) -> metadata.serviceSubTypes)
        pure $ (info {serviceSubTypes = mbServiceSubTypes} :: UpcomingVehicleInfo)
      else pure info

  logDebug $ "tripTimingsWithCalendars after filtering on current time : " <> show tripTimingsWithSubTypes
  let upcomingBuses = sortBy (\a b -> compare (arrivalTimeInSeconds a) (arrivalTimeInSeconds b)) tripTimingsWithSubTypes

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

measureLatency :: MonadFlow m => m a -> Text -> m a
measureLatency action label = do
  startTime <- getCurrentTime
  result <- withLogTag label action
  endTime <- getCurrentTime
  let latency = diffUTCTime endTime startTime
  logDebug $ label <> " Latency: " <> show latency <> " seconds"
  return result

getBestOneWayRoute :: MultiModalTypes.GeneralVehicleType -> [MultiModalTypes.MultiModalRoute] -> Maybe Text -> Maybe Text -> Maybe MultiModalTypes.MultiModalRoute
getBestOneWayRoute vehicleCategory routes mbOriginStopCode mbDestinationStopCode = do
  let selectedVehicleCategoryRoutes = filter (\r -> onlySelectedModeWithWalkLegs vehicleCategory r.legs) routes
  firstJust
    [ findConditionalRoute [correctToFromStops mbOriginStopCode mbDestinationStopCode] selectedVehicleCategoryRoutes,
      listToMaybe selectedVehicleCategoryRoutes
    ]
  where
    removeWalkLegs = filter (\l -> l.mode /= MultiModal.Walk)
    onlySelectedModeWithWalkLegs vc = all (\l -> l.mode == vc) . removeWalkLegs
    correctToFromStops (Just originStopCode) (Just destinationStopCode) legs =
      case ((listToMaybe legs) >>= (.fromStopDetails) >>= (.stopCode), (listToMaybe $ reverse legs) >>= (.toStopDetails) >>= (.stopCode)) of
        (Just journeyStartStopCode, Just journeyEndStopCode) -> journeyStartStopCode == originStopCode && journeyEndStopCode == destinationStopCode
        _ -> False
    correctToFromStops _ _ _ = True

    findConditionalRoute fns xs = find (\r -> all (\fn -> fn r.legs) fns) xs
    firstJust xs = foldr (<|>) Nothing xs

mkMultiModalRoute :: UTCTime -> Maybe MultiModalTypes.MultiModalLeg -> MultiModalTypes.GeneralVehicleType -> NonEmpty MultiModalTypes.MultiModalRouteDetails -> Maybe HighPrecMeters -> Maybe Text -> MultiModalTypes.MultiModalRoute
mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode routeDetails mbDistance mbProviderRouteId = do
  let firstRouteDetails = NonEmpty.head routeDetails
  let lastRouteDetails = NonEmpty.last routeDetails
  let duration = nominalDiffTimeToSeconds $ diffUTCTime (fromMaybe currentTimeIST lastRouteDetails.toArrivalTime) (fromMaybe currentTimeIST firstRouteDetails.fromArrivalTime)
  let distance = convertHighPrecMetersToDistance Meter $ fromMaybe (distanceBetweenInMeters (locationV2ToLatLong firstRouteDetails.startLocation) (locationV2ToLatLong lastRouteDetails.endLocation)) mbDistance
  let leg =
        MultiModalTypes.MultiModalLeg
          { distance = distance,
            duration = duration,
            polyline = Polyline {encodedPolyline = ""},
            mode,
            startLocation = firstRouteDetails.startLocation,
            endLocation = lastRouteDetails.endLocation,
            fromStopDetails = firstRouteDetails.fromStopDetails,
            toStopDetails = lastRouteDetails.toStopDetails,
            routeDetails = NonEmpty.toList routeDetails,
            serviceTypes = [],
            agency = Nothing,
            fromArrivalTime = firstRouteDetails.fromArrivalTime,
            fromDepartureTime = firstRouteDetails.fromDepartureTime,
            toArrivalTime = lastRouteDetails.toArrivalTime,
            toDepartureTime = lastRouteDetails.toDepartureTime,
            entrance = Nothing,
            exit = Nothing,
            providerRouteId = mbProviderRouteId
          }
  let legs = maybe [leg] (\pl -> [pl, leg]) mbPreliminaryLeg
  MultiModalTypes.MultiModalRoute
    { distance = distance,
      duration = duration,
      startTime = leg.fromArrivalTime,
      endTime = leg.toArrivalTime,
      legs = legs,
      relevanceScore = Nothing
    }
  where
    locationV2ToLatLong :: LocationV2 -> LatLong
    locationV2ToLatLong locationV2 = LatLong {lat = locationV2.latLng.latitude, lon = locationV2.latLng.longitude}

buildOneWayBusScanRouteDetails ::
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
  Text ->
  Maybe DIntegratedBPPConfig.IntegratedBPPConfig ->
  m [MultiModalTypes.MultiModalRoute]
buildOneWayBusScanRouteDetails _ _ _ Nothing = return []
buildOneWayBusScanRouteDetails routeCode originStopCode destinationStopCode (Just integratedBppConfig) = mkMultiModalRoute' $ do
  fromStop <- OTPRest.getStationByGtfsIdAndStopCode originStopCode integratedBppConfig >>= fromMaybeM (StopNotFound $ "fromStopCode: " <> originStopCode)
  toStop <- OTPRest.getStationByGtfsIdAndStopCode destinationStopCode integratedBppConfig >>= fromMaybeM (StopNotFound $ "toStopCode: " <> destinationStopCode)
  mbRoute <- OTPRest.getRouteByRouteId integratedBppConfig routeCode
  case mbRoute of
    Nothing -> throwError (RouteNotFound routeCode)
    Just route -> do
      routeStopMappings <- OTPRest.getRouteStopMappingByRouteCode route.code integratedBppConfig
      let estimatedDistance =
            fst $
              foldl'
                ( \(acc, mbPrev) routeStopMapping ->
                    case mbPrev of
                      Just prev -> do
                        let distance = distanceBetweenInMeters prev.stopPoint routeStopMapping.stopPoint
                        (acc + distance, Just routeStopMapping)
                      Nothing -> (acc, Nothing)
                )
                (0, listToMaybe routeStopMappings)
                routeStopMappings
      let estimatedDurationInSeconds = fromIntegral $ div (max 1 (highPrecMetersToMeters estimatedDistance).getMeters) 7 -- 7m/s (25 km/h)
      now <- getCurrentTime
      let fromStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just fromStop.code,
                platformCode = Nothing,
                name = Just fromStop.name,
                gtfsId = Just fromStop.code
              }
      let toStopDetails =
            MultiModalTypes.MultiModalStopDetails
              { stopCode = Just toStop.code,
                platformCode = Nothing,
                name = Just toStop.name,
                gtfsId = Just toStop.code
              }
      case (fromStop.lat, fromStop.lon, toStop.lat, toStop.lon) of
        (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
          let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = fromStopLat, longitude = fromStopLon}}
          let toStopLocation = LocationV2 {latLng = LatLngV2 {latitude = toStopLat, longitude = toStopLon}}
          return $
            MultiModalTypes.MultiModalRouteDetails
              { gtfsId = Just route.code,
                longName = Just route.longName,
                shortName = Just route.shortName,
                alternateShortNames = [],
                color = Nothing,
                fromStopDetails = Just fromStopDetails,
                toStopDetails = Just toStopDetails,
                startLocation = fromStopLocation,
                endLocation = toStopLocation,
                subLegOrder = 1, -- Starts from 1
                fromArrivalTime = Just now,
                fromDepartureTime = Just now,
                toArrivalTime = Just (addUTCTime estimatedDurationInSeconds now),
                toDepartureTime = Just (addUTCTime estimatedDurationInSeconds now)
              }
        _ -> throwError (StopDoesNotHaveLocation $ "from: " <> show fromStop <> " to: " <> show toStop)
  where
    mkMultiModalRoute' routeDetailsM = do
      currentTime <- getCurrentTime
      routeDetails <- routeDetailsM
      let (_, currentTimeIST) = getISTTimeInfo currentTime
      let mode = MultiModalTypes.Bus
      return $ [mkMultiModalRoute currentTimeIST Nothing mode (NonEmpty.fromList [routeDetails]) Nothing Nothing]

buildMultimodalRouteDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  Int ->
  Maybe Text ->
  Text ->
  Text ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  Bool ->
  Bool ->
  m (Maybe MultiModalTypes.MultiModalRouteDetails, Maybe HighPrecMeters)
buildMultimodalRouteDetails subLegOrder mbRouteCode originStopCode destinationStopCode integratedBppConfig mid mocid vc fetchTimings calledForSubwaySingleMode = do
  mbFromStop <- OTPRest.getStationByGtfsIdAndStopCode originStopCode integratedBppConfig
  mbToStop <- OTPRest.getStationByGtfsIdAndStopCode destinationStopCode integratedBppConfig
  currentTime <- getCurrentTime
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  case (mbFromStop >>= (.lat), mbFromStop >>= (.lon), mbToStop >>= (.lat), mbToStop >>= (.lon)) of
    (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
      (nextAvailableRouteCode, possibleRoutes, originStopTimings) <-
        measureLatency
          ( bool
              ( do
                  validRoutes <- getRouteCodesFromTo originStopCode destinationStopCode integratedBppConfig
                  validRouteDetails <- OTPRest.getRoutesByRouteIds integratedBppConfig validRoutes
                  let availableRoutesByTier =
                        [ AvailableRoutesByTier
                            { availableRoutes = nub $ map (.shortName) validRouteDetails,
                              availableRoutesInfo = map (\routeDetail -> AvailableRoutesInfo {shortName = routeDetail.shortName, longName = routeDetail.longName, routeCode = routeDetail.code, routeTimings = [], isLiveTrackingAvailable = False, source = GTFS}) validRouteDetails,
                              fare = PriceAPIEntity {amount = 0.0, currency = INR},
                              nextAvailableBuses = [],
                              nextAvailableTimings = [],
                              quoteId = Nothing,
                              serviceTier = Spec.ORDINARY,
                              serviceTierDescription = Nothing,
                              serviceTierName = Nothing,
                              source = GTFS,
                              trainTypeCode = Nothing,
                              ticketTypeCode = Nothing,
                              via = Nothing,
                              alsoValidServiceTypes = Nothing
                            }
                        ]
                  return (listToMaybe validRoutes, availableRoutesByTier, [])
              )
              (findPossibleRoutes Nothing originStopCode destinationStopCode currentTime integratedBppConfig mid mocid vc True True calledForSubwaySingleMode False)
              fetchTimings
          )
          "findPossibleRoutesMaybeFetchTimings"
      let originStopTripId = listToMaybe originStopTimings >>= (\timing -> Just timing.tripId)
      let distance = distanceBetweenInMeters (LatLong fromStopLat fromStopLon) (LatLong toStopLat toStopLon)
      let routeCode = mbRouteCode <|> nextAvailableRouteCode
      mbRoute <-
        maybe
          (return Nothing)
          (\rc -> OTPRest.getRouteByRouteId integratedBppConfig rc)
          routeCode
      case mbRoute of
        Just route -> do
          routeStopMappings <- OTPRest.getRouteStopMappingByRouteCode route.code integratedBppConfig
          -- Get timing information for this route at the origin stop
          tripInfo' <- maybe (return Nothing) (\tripId -> measureLatency (OTPRest.getNandiTripInfo integratedBppConfig tripId.getId) "getNandiTripInfo") originStopTripId
          let destinationArrivalTime' = tripInfo' >>= \tripInfo -> fmap secondsToTime $ find (\schedule -> schedule.stopCode == destinationStopCode) tripInfo.schedule >>= Just . (.arrivalTime)
              destinationDepartureTime' = tripInfo' >>= \tripInfo -> fmap secondsToTime $ find (\schedule -> schedule.stopCode == destinationStopCode) tripInfo.schedule >>= Just . (.departureTime)
          destStopTimings <- case (mbRouteCode, tripInfo', destinationArrivalTime', destinationDepartureTime') of
            (Nothing, Just tripInfo, Just destinationArrivalTime, Just destinationDepartureTime) -> do
              logDebug $ "destinationArrivalTime: " <> show destinationArrivalTime <> " destinationDepartureTime: " <> show destinationDepartureTime <> " tripInfo: " <> show tripInfo
              return $
                [ RouteStopTimeTable
                    { integratedBppConfigId = integratedBppConfig.id,
                      tripId = Id tripInfo.tripId,
                      stopCode = destinationStopCode,
                      timeOfArrival = destinationArrivalTime,
                      timeOfDeparture = destinationDepartureTime,
                      predictedArrivalTime = Nothing,
                      routeCode = route.code,
                      serviceTierType = Spec.SECOND_CLASS,
                      serviceTierName = Just "SECOND_CLASS",
                      delay = Nothing,
                      source = GTFS,
                      stage = Nothing,
                      platformCode = Nothing,
                      providerStopCode = Nothing,
                      isStageStop = Nothing,
                      merchantId = Just mid,
                      merchantOperatingCityId = Just mocid,
                      createdAt = currentTime,
                      updatedAt = currentTime
                    }
                ]
            -- fetchLiveTimings [route.code] destinationStopCode currentTime integratedBppConfig mid mocid vc True calledForSubwaySingleMode
            _ -> do
              logDebug $ "fetching old traditional way using live timings"
              fetchLiveTimings [route.code] destinationStopCode currentTime currentTimeIST integratedBppConfig mid mocid vc True calledForSubwaySingleMode

          let stopCodeToSequenceNum = Map.fromList $ map (\rst -> (rst.stopCode, rst.sequenceNum)) routeStopMappings

          let mbEarliestOriginTiming =
                findEarliestTiming currentTimeIST currentTime $ originStopTimings

          let mbDestinationTiming = do
                originTiming <- mbEarliestOriginTiming
                findMatchingDestinationTiming (.tripId) (.stopCode) originTiming destStopTimings stopCodeToSequenceNum

          let mbFirstOriginTiming = listToMaybe originStopTimings
          let mbFirstDestinationTiming = do
                originTiming <- mbFirstOriginTiming
                findMatchingDestinationTiming (.tripId) (.stopCode) originTiming destStopTimings stopCodeToSequenceNum

          let mbDepartureTime = getUTCArrivalTime . (.timeOfDeparture) <$> mbEarliestOriginTiming <*> pure currentTime
              mbArrivalTime = getUTCArrivalTime . (.timeOfArrival) <$> mbDestinationTiming <*> pure currentTime
              mbOriginPlatformCode = ((.platformCode) =<< mbEarliestOriginTiming) <|> ((.platformCode) =<< mbFirstOriginTiming) -- (.platformCode) =<< mbEarliestOriginTiming
              mbDestinationPlatformCode = ((.platformCode) =<< mbDestinationTiming) <|> ((.platformCode) =<< mbFirstDestinationTiming)
          let fromStopDetails =
                MultiModalTypes.MultiModalStopDetails
                  { stopCode = mbFromStop <&> (.code),
                    platformCode = mbOriginPlatformCode,
                    name = mbFromStop <&> (.name),
                    gtfsId = mbFromStop <&> (.code)
                  }
          let toStopDetails =
                MultiModalTypes.MultiModalStopDetails
                  { stopCode = mbToStop <&> (.code),
                    platformCode = mbDestinationPlatformCode,
                    name = mbToStop <&> (.name),
                    gtfsId = mbToStop <&> (.code)
                  }
          let fromStopLocation = LocationV2 {latLng = LatLngV2 {latitude = fromStopLat, longitude = fromStopLon}}
          let toStopLocation = LocationV2 {latLng = LatLngV2 {latitude = toStopLat, longitude = toStopLon}}
          return $
            ( Just
                MultiModalTypes.MultiModalRouteDetails
                  { gtfsId = Just route.code,
                    longName = Just route.longName,
                    shortName = Just route.shortName,
                    alternateShortNames = concatMap (.availableRoutes) possibleRoutes,
                    color = route.color,
                    fromStopDetails = Just fromStopDetails,
                    toStopDetails = Just toStopDetails,
                    startLocation = fromStopLocation,
                    endLocation = toStopLocation,
                    subLegOrder,
                    fromArrivalTime = mbDepartureTime,
                    fromDepartureTime = mbDepartureTime,
                    toArrivalTime = mbArrivalTime,
                    toDepartureTime = mbArrivalTime
                  },
              Just distance
            )
        _ -> return (Nothing, Just distance)
    _ -> return (Nothing, Nothing)
  where
    findMatchingDestinationTiming getTripId getStopCode originTiming destStopTimings stopCodeToSequenceNum =
      let originSeq = Map.lookup (getStopCode originTiming) stopCodeToSequenceNum
       in find
            ( \dt ->
                getTripId dt == getTripId originTiming
                  && case (Map.lookup (getStopCode dt) stopCodeToSequenceNum, originSeq) of
                    (Just destSeq, Just oSeq) -> destSeq > oSeq
                    _ -> False
            )
            destStopTimings

buildSingleModeDirectRoutes ::
  (Maybe Text -> LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  MultiModalTypes.GeneralVehicleType ->
  Flow [MultiModalTypes.MultiModalRoute]
buildSingleModeDirectRoutes getPreliminaryLeg mbRouteCode (Just originStopCode) (Just destinationStopCode) (Just integratedBppConfig) mid mocid vc mode = do
  (mbRouteDetails, _) <- buildMultimodalRouteDetails 1 mbRouteCode originStopCode destinationStopCode integratedBppConfig mid mocid vc True False
  case mbRouteDetails of
    Just routeDetails -> do
      currentTime <- getCurrentTime
      let (_, currentTimeIST) = getISTTimeInfo currentTime
      mbPreliminaryLeg <- getPreliminaryLeg (routeDetails.fromStopDetails >>= (.name)) routeDetails.startLocation
      return [mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode (NonEmpty.fromList [routeDetails]) Nothing Nothing]
    Nothing -> return []
buildSingleModeDirectRoutes _ _ _ _ _ _ _ _ _ = return []

getSubwayValidRoutes ::
  [ViaRouteDetails] ->
  (Maybe Text -> LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)) ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  MultiModalTypes.GeneralVehicleType ->
  Bool ->
  Flow ([MultiModalTypes.MultiModalRoute], [ViaRouteDetails])
getSubwayValidRoutes allSubwayRoutes getPreliminaryLeg integratedBppConfig mid mocid vc mode processAllViaPoints = do
  if processAllViaPoints
    then do
      allRoutes <-
        mapMaybeM
          processRoute
          allSubwayRoutes
      return (allRoutes, [])
    else do
      let isCorridorIssueRoute vr =
            let pairs = vr.viaPoints
                rawIssue (f, t) =
                  let fc = f `elem` corridorStations
                      tc = t `elem` corridorStations
                   in (fc || tc) && not (fc && tc)
                isBridged idx (f, t) =
                  let fc = f `elem` corridorStations
                      tc = t `elem` corridorStations
                      prevPair = if idx > 0 then Just (pairs !! (idx - 1)) else Nothing
                      nextPair = if idx < length pairs - 1 then Just (pairs !! (idx + 1)) else Nothing
                      toBridged = tc && maybe False (\(_, nt) -> nt `elem` corridorStations) nextPair
                      fromBridged = fc && maybe False (\(pf, _) -> pf `elem` corridorStations) prevPair
                   in toBridged || fromBridged
             in any (\(idx, pair) -> rawIssue pair && not (isBridged idx pair)) (zip [0 ..] pairs)
      let (normalRoutes, corridorRoutes) =
            if deprioritizeEnabled && not (null corridorStations)
              then partition (not . isCorridorIssueRoute) allSubwayRoutes
              else (allSubwayRoutes, [])
      let orderedRoutes =
            if null normalRoutes || null corridorRoutes
              then allSubwayRoutes -- either deprioritization is off, or all routes are corridor-issue (process as-is)
              else normalRoutes ++ corridorRoutes
      when (not (null corridorRoutes) && not (null normalRoutes)) $
        logDebug $ "corridorDeprioritization: reordered " <> show (length corridorRoutes) <> " corridor-issue route(s) to end"
      go orderedRoutes
  where
    corridorStations :: [Text]
    corridorStations =
      case integratedBppConfig.providerConfig of
        DIntegratedBPPConfig.CRIS config -> fromMaybe [] config.corridorStations
        _ -> []
    deprioritizeEnabled :: Bool
    deprioritizeEnabled =
      case integratedBppConfig.providerConfig of
        DIntegratedBPPConfig.CRIS config -> fromMaybe False config.enableCorridorDeprioritization
        _ -> False
    tryCorridorAlts :: [Text] -> (Text -> Flow (Maybe a)) -> Flow (Maybe a)
    tryCorridorAlts [] _ = return Nothing
    tryCorridorAlts (x : xs) f =
      f x >>= \case
        Just r -> return (Just r)
        Nothing -> tryCorridorAlts xs f
    getCorridorWalkResult :: Text -> Text -> Flow (Maybe MultiModalTypes.MultiModalRouteDetails, Maybe HighPrecMeters)
    getCorridorWalkResult fromStopCode toStopCode = do
      mbFromStop <- OTPRest.getStationByGtfsIdAndStopCode fromStopCode integratedBppConfig
      mbToStop <- OTPRest.getStationByGtfsIdAndStopCode toStopCode integratedBppConfig
      case (mbFromStop >>= (.lat), mbFromStop >>= (.lon), mbToStop >>= (.lat), mbToStop >>= (.lon)) of
        (Just fromLat, Just fromLon, Just toLat, Just toLon) ->
          return (Nothing, Just $ distanceBetweenInMeters (LatLong fromLat fromLon) (LatLong toLat toLon))
        _ -> return (Nothing, Nothing)
    processRoute viaRoute = do
      disableViaPointTimetableCheck <- asks (.disableViaPointTimetableCheck)
      let viaPoints = viaRoute.viaPoints
          routeDistance = metersToHighPrecMeters viaRoute.distance
      routeDetailsWithDistance <- mapM (\(osc, dsc) -> measureLatency (buildMultimodalRouteDetails 1 Nothing osc dsc integratedBppConfig mid mocid vc disableViaPointTimetableCheck True) "buildMultimodalRouteDetails") viaPoints
      logDebug $ "buildTrainAllViaRoutes routeDetailsWithDistance: " <> show routeDetailsWithDistance
      -- ensure that atleast one train route is possible or two stops are less than 1km apart so that user can walk to other station e.g. Chennai Park to Central station
      let singleModeWalkThreshold =
            fromMaybe
              1000
              ( case integratedBppConfig.providerConfig of
                  DIntegratedBPPConfig.CRIS config -> fromIntegral <$> config.singleModeWalkThreshold
                  _ -> Nothing
              )
      let isPairValid (mbRD, mbDist) = isJust mbRD || (isJust mbDist && mbDist < Just (HighPrecMeters singleModeWalkThreshold))
      let isInitialRoutePossible = all isPairValid routeDetailsWithDistance
      (_, finalRouteDetailsWithDistance) <-
        if isInitialRoutePossible || null corridorStations
          then return (viaPoints, routeDetailsWithDistance)
          else do
            expandedPairs <-
              forM (zip viaPoints routeDetailsWithDistance) $ \((from, to), result) ->
                if isPairValid result
                  then return [((from, to), result)]
                  else do
                    let fromIsCorr = from `elem` corridorStations
                        toIsCorr = to `elem` corridorStations
                    case (fromIsCorr, toIsCorr) of
                      (True, True) -> do
                        unless (isPairValid result) $
                          logDebug $ "corridorExpansion: walk leg between corridor stations (" <> from <> "," <> to <> ") exceeds singleModeWalkThreshold — check corridorStations config"
                        return [((from, to), result)]
                      (True, False) -> do
                        -- 'from' is corridor station; try other corridor members as intermediate.
                        let alts = filter (/= from) corridorStations
                        mbExpanded <-
                          tryCorridorAlts alts $ \alt -> do
                            trainResult@(mbRD2, _) <- buildMultimodalRouteDetails 1 Nothing alt to integratedBppConfig mid mocid vc disableViaPointTimetableCheck True
                            if isJust mbRD2
                              then do
                                walkResult <- getCorridorWalkResult from alt
                                if isPairValid walkResult
                                  then do
                                    logDebug $ "corridorExpansion (from): expanded (" <> from <> "," <> to <> ") via " <> alt
                                    return $ Just [((from, alt), walkResult), ((alt, to), trainResult)]
                                  else return Nothing
                              else return Nothing
                        return $ fromMaybe [((from, to), result)] mbExpanded
                      (False, True) -> do
                        -- 'to' is corridor station; try other corridor members as intermediate.
                        let alts = filter (/= to) corridorStations
                        mbExpanded <-
                          tryCorridorAlts alts $ \alt -> do
                            trainResult@(mbRD2, _) <- buildMultimodalRouteDetails 1 Nothing from alt integratedBppConfig mid mocid vc disableViaPointTimetableCheck True
                            if isJust mbRD2
                              then do
                                walkResult <- getCorridorWalkResult alt to
                                if isPairValid walkResult
                                  then do
                                    logDebug $ "corridorExpansion (to): expanded (" <> from <> "," <> to <> ") via " <> alt
                                    return $ Just [((from, alt), trainResult), ((alt, to), walkResult)]
                                  else return Nothing
                              else return Nothing
                        return $ fromMaybe [((from, to), result)] mbExpanded
                      (False, False) ->
                        -- No corridor station involved; nothing to expand
                        return [((from, to), result)]
            let flatPairs = concat expandedPairs
            return (map fst flatPairs, map snd flatPairs)
      let isRoutePossible = all isPairValid finalRouteDetailsWithDistance
      if isRoutePossible
        then do
          let routeDetails = mapMaybe (\(mbRouteDetails, _) -> mbRouteDetails) finalRouteDetailsWithDistance
          logDebug $ "buildTrainAllViaRoutes routeDetails: " <> show routeDetails
          let updateRouteDetails = zipWith (\idx routeDetail -> routeDetail {MultiModalTypes.subLegOrder = idx}) [1 ..] routeDetails
          case updateRouteDetails of
            [] -> return Nothing
            (rD : _) -> do
              currentTime <- getCurrentTime
              let (_, currentTimeIST) = getISTTimeInfo currentTime
              mbPreliminaryLeg <- getPreliminaryLeg (rD.fromStopDetails >>= (.name)) rD.startLocation
              return $ Just $ mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode (NonEmpty.fromList updateRouteDetails) (Just routeDistance) (Just viaRoute.routeCode)
        else return Nothing
    go [] = return ([], [])
    go (x : xs) = do
      processRoute x >>= \case
        Just route -> do
          return ([route], x : xs)
        Nothing -> do
          logDebug $ "getSubwayValidRoutes go Nothing: " <> show x <> " so going for other path"
          go xs

buildTrainAllViaRoutes ::
  (Maybe Text -> LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)) ->
  Maybe Text ->
  Maybe Text ->
  Maybe DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Enums.VehicleCategory ->
  MultiModalTypes.GeneralVehicleType ->
  Bool ->
  Id Person ->
  Text ->
  [Spec.ServiceTierType] ->
  [DQuote.FRFSQuoteType] ->
  Flow ([MultiModalTypes.MultiModalRoute], [ViaRouteDetails])
buildTrainAllViaRoutes getPreliminaryLeg (Just originStopCode) (Just destinationStopCode) (Just integratedBppConfig) mid mocid vc mode processAllViaPoints personId searchReqId blacklistedServiceTiers blacklistedFareQuoteTypes = do
  eitherAllSubwayRoutes <- withTryCatch "getAllSubwayRoutes:buildTrainAllViaRoutes" (measureLatency getAllSubwayRoutes "getAllSubwayRoutes")
  case eitherAllSubwayRoutes of
    Right allSubwayRoutes -> measureLatency (getSubwayValidRoutes allSubwayRoutes getPreliminaryLeg integratedBppConfig mid mocid vc mode processAllViaPoints) "getSubwayValidRoutes"
    Left err -> do
      logError $ "Error in getAllSubwayRoutes: " <> show err
      return ([], [])
  where
    getAllSubwayRoutes :: Flow [ViaRouteDetails]
    getAllSubwayRoutes = do
      case integratedBppConfig.providerConfig of
        DIntegratedBPPConfig.CRIS crisConfig -> do
          let fareRoute = CallAPI.FareRoute {segments = pure CallAPI.BasicRouteDetail {routeCode = "-", startStopCode = originStopCode, endStopCode = destinationStopCode}, mbProviderRouteId = Nothing}
          (_, fares) <- Flow.getFares personId mid mocid integratedBppConfig fareRoute (Utils.becknVehicleCategoryToFrfsVehicleCategory vc) Nothing (Just searchReqId) blacklistedServiceTiers blacklistedFareQuoteTypes True True
          let filteredFares =
                filter
                  ( \fare ->
                      notElem fare.vehicleServiceTier.serviceTierType blacklistedServiceTiers
                        && maybe True (\ft -> notElem ft blacklistedFareQuoteTypes) (fare.fareQuoteType)
                  )
                  fares

          let compareViaRoutes a b = case crisConfig.routeSortingCriteria of
                Just DIntegratedBPPConfig.FARE -> compare a.fare b.fare <> compare a.distance b.distance
                Just DIntegratedBPPConfig.DISTANCE -> compare a.distance b.distance <> compare a.fare b.fare
                Nothing -> EQ

          let viaRouteDetailsMap :: M.Map Text ViaRouteDetails
              viaRouteDetailsMap =
                foldl'
                  ( \acc fare -> case fare.fareDetails of
                      Nothing -> acc
                      Just fd ->
                        let viaStops = if T.strip fd.via == "" then [] else T.splitOn "-" (T.strip fd.via)
                            stops = [originStopCode] <> viaStops <> [destinationStopCode]
                            viaPoints = zipWith (,) stops (drop 1 stops)
                            fareAmount =
                              fromMaybe (HighPrecMoney 0.0) $
                                (find ((== ADULT) . (.category)) fare.categories) <&> (.price.amount)
                            newViaRoute =
                              ViaRouteDetails
                                { viaPoints = viaPoints,
                                  distance = fd.distance,
                                  routeCode = fd.providerRouteId,
                                  fare = fareAmount
                                }
                         in M.insertWith
                              (\new old -> if compareViaRoutes new old == LT then new else old)
                              fd.providerRouteId
                              newViaRoute
                              acc
                  )
                  M.empty
                  filteredFares

          let viaRouteDetails = sortBy compareViaRoutes (M.elems viaRouteDetailsMap)
          logDebug $ "getAllSubwayRoutes viaRouteDetails: " <> show viaRouteDetails
          return viaRouteDetails
        _ -> return []
buildTrainAllViaRoutes _ _ _ _ _ _ _ _ _ _ _ _ _ = return ([], [])

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
  journeyLegs <- QJourneyLeg.getJourneyLegs journey.id
  let onlyPublicTransportLegs = filter (\leg -> leg.mode `elem` [DTrip.Bus, DTrip.Metro, DTrip.Subway]) journeyLegs
  fare <-
    foldrM
      ( \leg acc -> do
          case leg.legPricingId of
            Just pricingId -> do
              quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId (Id pricingId)
              let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
                  adultUnitPrice = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice.amount)
              return $ acc + fromMaybe 0 adultUnitPrice
            Nothing -> return acc
      )
      (HighPrecMoney 0.0)
      onlyPublicTransportLegs
  now <- getCurrentTime
  let legs = sortBy (comparing (.sequenceNumber)) onlyPublicTransportLegs
  let mbFirstLeg = listToMaybe legs
  let mbLastLeg = listToMaybe (reverse legs)
  let mbFirstStopCode = mbFirstLeg >>= (.fromStopDetails) >>= (.stopCode)
  let mbLastStopCode = mbLastLeg >>= (.toStopDetails) >>= (.stopCode)
  let mbRouteCode = mbFirstLeg <&> (.routeDetails) >>= listToMaybe >>= (.routeGtfsId)
  let mbEndLocation = mbLastLeg <&> (.endLocation)
  case (mbFirstLeg, mbFirstStopCode, mbLastStopCode, mbEndLocation) of
    (Just firstLeg, Just firstStopCode, Just lastStopCode, Just endLocation) -> do
      uuid <- generateGUID
      let cityId = journey.merchantOperatingCityId
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
                    address = UCommon.mkAddress <$> (journey.toLocation <&> (.address)),
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

postMultimodalPaymentUpdateOrderUtil :: (ServiceFlow m r, EncFlow m r, EsqDBReplicaFlow m r, HasField "isMetroTestTransaction" r Bool) => TPayment.PaymentServiceType -> Person -> Id Merchant -> Id MerchantOperatingCity -> [DFRFSTicketBooking.FRFSTicketBooking] -> Maybe Bool -> Bool -> m (Maybe DOrder.PaymentOrder)
postMultimodalPaymentUpdateOrderUtil paymentType person merchantId merchantOperatingCityId bookings mbEnableOffer isMockPayment = do
  frfsConfig <-
    getConfig (FRFSConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show person.merchantOperatingCityId)
  frfsBookingsPayments <- mapMaybeM QFRFSTicketBookingPayment.findTicketBookingPayment bookings
  (vendorSplitDetails, amountUpdated) <- createVendorSplitFromBookings bookings merchantId person.merchantOperatingCityId paymentType frfsConfig.isFRFSTestingEnabled
  isSplitEnabled <- TPayment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing paymentType -- TODO :: You can be moved inside :)
  isPercentageSplitEnabled <- TPayment.getIsPercentageSplit merchantId merchantOperatingCityId Nothing paymentType
  let isSingleMode = case bookings of
        [_] -> True
        _ -> False
  splitDetails <- TPayment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amountUpdated vendorSplitDetails isPercentageSplitEnabled isSingleMode
  baskets <- createBasketFromBookings bookings merchantId merchantOperatingCityId paymentType mbEnableOffer
  let splitDetailsAmount = TPayment.extractSplitSettlementDetailsAmount splitDetails
  if frfsConfig.canUpdateExistingPaymentOrder
    then do
      case listToMaybe frfsBookingsPayments of
        Nothing -> return Nothing
        Just frfsBookingPayment -> do
          paymentOrder <- QOrder.findById frfsBookingPayment.paymentOrderId >>= fromMaybeM (PaymentOrderNotFound frfsBookingPayment.paymentOrderId.getId)
          let updateReq =
                KT.OrderUpdateReq
                  { amount = amountUpdated,
                    orderShortId = paymentOrder.shortId.getShortId,
                    splitSettlementDetails = splitDetailsAmount
                  }
          _ <- TPayment.updateOrder person.merchantId person.merchantOperatingCityId Nothing TPayment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion Nothing updateReq
          QOrder.updateAmount paymentOrder.id amountUpdated
          let updatedOrder :: DOrder.PaymentOrder
              updatedOrder = paymentOrder {DOrder.amount = amountUpdated}
          return $ Just updatedOrder
    else createPaymentOrder bookings merchantOperatingCityId merchantId amountUpdated person paymentType vendorSplitDetails (Just baskets) isMockPayment

makePossibleRoutesKey :: Text -> Text -> Id DIntegratedBPPConfig.IntegratedBPPConfig -> Text
makePossibleRoutesKey fromCode toCode integratedBPPConfig = "PossibleRoutes:" <> fromCode <> ":" <> toCode <> ":" <> integratedBPPConfig.getId

fetchPossibleRoutes :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => Text -> Text -> DIntegratedBPPConfig.IntegratedBPPConfig -> m [Text]
fetchPossibleRoutes fromCode toCode integratedBPPConfig = do
  -- Get route mappings that contain the origin stop
  fromRouteStopMappings <- OTPRest.getRouteStopMappingByStopCode fromCode integratedBPPConfig

  -- Get route mappings that contain the destination stop
  toRouteStopMappings <- OTPRest.getRouteStopMappingByStopCode toCode integratedBPPConfig

  -- Find common routes that have both the origin and destination stops
  -- and ensure that from-stop comes before to-stop in the route sequence
  let fromRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) fromRouteStopMappings
      toRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) toRouteStopMappings
      validRoutes =
        nub $
          [ fromRouteCode
            | (fromRouteCode, fromSeq') <- fromRouteStopMap,
              (toRouteCode, toSeq') <- toRouteStopMap,
              fromRouteCode == toRouteCode && fromSeq' < toSeq' -- Ensure correct sequence
          ]
  pure validRoutes

getRouteCodesFromTo :: (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasShortDurationRetryCfg r c) => Text -> Text -> DIntegratedBPPConfig.IntegratedBPPConfig -> m [Text]
getRouteCodesFromTo fromCode toCode integratedBPPConfig = do
  let key = makePossibleRoutesKey fromCode toCode integratedBPPConfig.id
  mbRoutes <- Hedis.safeGet key
  case mbRoutes of
    Just routeCodes -> return routeCodes
    Nothing -> do
      routeCodes <- fetchPossibleRoutes fromCode toCode integratedBPPConfig
      Hedis.setExp key routeCodes 86400 -- 24 hours
      return routeCodes

-- | Find adjacent legs based on sequence number
findAdjacentLegs :: Int -> [DJourneyLeg.JourneyLeg] -> (Maybe DJourneyLeg.JourneyLeg, Maybe DJourneyLeg.JourneyLeg)
findAdjacentLegs sequenceNumber legs =
  let sortedLegs = sortBy (comparing (.sequenceNumber)) legs
      prevLeg = find (\leg -> leg.sequenceNumber == sequenceNumber - 1) sortedLegs
      nextLeg = find (\leg -> leg.sequenceNumber == sequenceNumber + 1) sortedLegs
   in (prevLeg, nextLeg)

-- | Get distance and duration between two locations using OSRM
getDistanceAndDuration ::
  (CacheFlow m r, EncFlow m r, EsqDBFlow m r, MonadFlow m, HasKafkaProducer r) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  LatLong ->
  LatLong ->
  m (Maybe Meters, Maybe Seconds)
getDistanceAndDuration merchantId merchantOpCityId startLocation endLocation = do
  let origin = startLocation
      destination = endLocation
  distResp <-
    withTryCatch "getMultimodalWalkDistance:getDistanceAndDuration" $
      Maps.getMultimodalWalkDistance merchantId merchantOpCityId Nothing $
        Maps.GetDistanceReq
          { origin = origin,
            destination = destination,
            travelMode = Just Maps.FOOT,
            sourceDestinationMapping = Nothing,
            distanceUnit = Meter
          }
  case distResp of
    Right response -> do
      return (Just response.distance, Just response.duration)
    Left err -> do
      logError $ "Failed to get walk distance from OSRM for leg " <> show merchantId.getId <> ": " <> show err
      -- Return Nothing instead of throwing error to allow graceful fallback
      return (Nothing, Nothing)

data VehicleLiveRouteInfo = VehicleLiveRouteInfo
  { routeCode :: Maybe Text,
    serviceType :: Spec.ServiceTierType,
    vehicleNumber :: Text,
    routeNumber :: Maybe Text,
    waybillId :: Maybe Text,
    scheduleNo :: Maybe Text,
    tripNumber :: Maybe Int,
    depot :: Maybe Text,
    remaining_trip_details :: Maybe [NandiTypes.BusScheduleTrip],
    isActuallyValid :: Maybe Bool,
    busConductorId :: Maybe Text,
    busDriverId :: Maybe Text,
    busTagNumber :: Maybe Text,
    eligiblePassIds :: Maybe [Text],
    serviceSubTypes :: Maybe [Spec.ServiceSubType],
    seatLayoutId :: Maybe (Id SeatLayout.SeatLayout)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getVehicleLiveRouteInfo ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  Maybe Bool ->
  m (Maybe (DIntegratedBPPConfig.IntegratedBPPConfig, VehicleLiveRouteInfo))
getVehicleLiveRouteInfo integratedBPPConfigs vehicleNumber mbPassVerifyReq = do
  eitherResult <- withTryCatch "getVehicleLiveRouteInfoUnsafe:getVehicleLiveRouteInfo" (getVehicleLiveRouteInfoUnsafe integratedBPPConfigs vehicleNumber mbPassVerifyReq)
  case eitherResult of
    Left err -> do
      logError $ "Error getting vehicle live route info: " <> show err
      return Nothing
    Right result -> return result

getVehicleServiceTypeFromInMem ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  m (Maybe Spec.ServiceTierType)
getVehicleServiceTypeFromInMem integratedBPPConfigs vehicleNumber = IM.withInMemCache ["CACHED_VEHICLE_TYPE", vehicleNumber] 43200 $ do
  mbMbResult <-
    SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs $ \config ->
      OTPRest.getVehicleMetadata config vehicleNumber Nothing
  pure $ join mbMbResult <&> (.serviceType)

-- | Get service subtypes for a vehicle, cached in-memory for 1 day (86400 seconds)
getVehicleServiceSubTypesFromInMem ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  m (Maybe [Spec.ServiceSubType])
getVehicleServiceSubTypesFromInMem integratedBPPConfigs vehicleNumber =
  IM.withInMemCache ["CACHED_VEHICLE_SERVICE_SUBTYPES", vehicleNumber] 86400 $ do
    mbMbResult <-
      SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs $ \config ->
        OTPRest.getVehicleMetadata config vehicleNumber Nothing
    pure $ join mbMbResult >>= (.serviceSubTypes)

getVehicleTagNumberFromInMem ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  m (Maybe Text)
getVehicleTagNumberFromInMem integratedBPPConfigs vehicleNumber =
  IM.withInMemCache ["CACHED_VEHICLE_TAG_NUMBER", vehicleNumber] 86400 $ do
    mbMbResult <-
      SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs $ \config ->
        OTPRest.getVehicleMetadata config vehicleNumber Nothing
    pure $ join mbMbResult >>= (.busTagNumber)

getVehicleMetadataFromInMem ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  m (Maybe (DIntegratedBPPConfig.IntegratedBPPConfig, NandiTypes.VehicleMetadataResponse))
getVehicleMetadataFromInMem integratedBPPConfigs vehicleNumber =
  IM.withInMemCache ["CACHED_VEHICLE_METADATA", vehicleNumber] 43200 $ do
    mbMbResult <-
      SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs $ \config ->
        (config,) <$> OTPRest.getVehicleMetadata config vehicleNumber Nothing
    pure $
      mbMbResult
        >>= \(integratedBPPConfig, mbResult) ->
          mbResult <&> (\result -> (integratedBPPConfig, result))

getVehicleLiveRouteInfoUnsafe ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  [DIntegratedBPPConfig.IntegratedBPPConfig] ->
  Text ->
  Maybe Bool ->
  m (Maybe (DIntegratedBPPConfig.IntegratedBPPConfig, VehicleLiveRouteInfo))
getVehicleLiveRouteInfoUnsafe integratedBPPConfigs vehicleNumber mbPassVerifyReq = do
  mbMbResult <-
    SIBC.fetchFirstIntegratedBPPConfigRightResult integratedBPPConfigs $ \config ->
      (config,) <$> OTPRest.getVehicleServiceType config vehicleNumber mbPassVerifyReq
  return $
    mbMbResult
      >>= \(integratedBPPConfig, mbResult) ->
        mbResult
          <&> ( \result ->
                  ( integratedBPPConfig,
                    VehicleLiveRouteInfo {routeNumber = result.route_number, vehicleNumber = vehicleNumber, routeCode = result.route_id, serviceType = result.service_type, waybillId = result.waybill_id, scheduleNo = result.schedule_no, depot = result.depot, isActuallyValid = result.is_actually_valid, remaining_trip_details = result.remaining_trip_details, tripNumber = result.trip_number, busConductorId = result.conductor_id, busDriverId = result.driver_id, busTagNumber = result.busTagNumber, eligiblePassIds = result.eligible_pass_ids, serviceSubTypes = result.service_sub_types, seatLayoutId = Id <$> result.seatLayoutId}
                  )
              )

sortAndGetBusFares :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => Id MerchantOperatingCity -> Maybe Spec.ServiceTierType -> [FRFSFare] -> m (Maybe FRFSFare)
sortAndGetBusFares _ _ [] = return Nothing
sortAndGetBusFares merchantOpCityId mbPreferredTier finalFares = do
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
  let cfgMap = maybe (toCfgMap defaultBusTierSortingConfig) toCfgMap (mbRiderConfig >>= (.busTierSortingConfig))
  let cfgMap' = FRFSUtils.adjustCfgMapForPreferredTier mbPreferredTier cfgMap
  let serviceTierTypeFromFare fare = Just fare.vehicleServiceTier.serviceTierType
  return $
    Just $
      minimumBy
        ( \fare1 fare2 ->
            compare
              (maybe maxBound (tierRank cfgMap') (serviceTierTypeFromFare fare1))
              (maybe maxBound (tierRank cfgMap') (serviceTierTypeFromFare fare2))
        )
        finalFares

defaultBusTierSortingConfig :: [RCTypes.BusTierSortingConfig]
defaultBusTierSortingConfig =
  [ RCTypes.BusTierSortingConfig 1 Spec.EXECUTIVE,
    RCTypes.BusTierSortingConfig 2 Spec.AC,
    RCTypes.BusTierSortingConfig 3 Spec.EXPRESS,
    RCTypes.BusTierSortingConfig 4 Spec.FIRST_CLASS,
    RCTypes.BusTierSortingConfig 5 Spec.NON_AC,
    RCTypes.BusTierSortingConfig 6 Spec.ORDINARY,
    RCTypes.BusTierSortingConfig 7 Spec.SECOND_CLASS,
    RCTypes.BusTierSortingConfig 8 Spec.SPECIAL,
    RCTypes.BusTierSortingConfig 9 Spec.THIRD_CLASS
  ]

tierRank :: M.Map Spec.ServiceTierType Int -> Spec.ServiceTierType -> Int
tierRank cfg tier = fromMaybe maxBound (M.lookup tier cfg)

toCfgMap :: [RCTypes.BusTierSortingConfig] -> M.Map Spec.ServiceTierType Int
toCfgMap xs = M.fromList [(RCTypes.tier x, RCTypes.rank x) | x <- xs]

-- Convert seconds from midnight to TimeOfDay format
-- Wraps around for next-day times (>= 24 hours)
secondsToTime :: Int -> LocalTime.TimeOfDay
secondsToTime seconds =
  let totalSeconds = seconds `mod` 86400 -- Wrap around after 24 hours (86400 seconds)
      hours :: Int = totalSeconds `div` 3600
      minutes :: Int = (totalSeconds `mod` 3600) `div` 60
      secs = fromIntegral $ totalSeconds `mod` 60
   in LocalTime.TimeOfDay hours minutes secs

getServiceTierFromQuote :: [DFRFSQuoteCategory.FRFSQuoteCategory] -> DFRFSQuote.FRFSQuote -> Maybe LegServiceTier
getServiceTierFromQuote quoteCategories quote = do
  let routeStations :: Maybe [FRFSTicketServiceAPI.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
      mbServiceTier = listToMaybe $ mapMaybe (.vehicleServiceTier) (fromMaybe [] routeStations)
      mbRouteCode = listToMaybe $ (.code) <$> (fromMaybe [] routeStations)
      fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
  mbServiceTier <&> \serviceTier -> do
    LegServiceTier
      { fare = mkPriceAPIEntity <$> (find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice)),
        quoteId = quote.id,
        routeCode = mbRouteCode,
        serviceTierName = serviceTier.shortName,
        serviceTierType = serviceTier._type,
        serviceTierDescription = serviceTier.description,
        via = quote.fareDetails <&> (.via),
        trainTypeCode = quote.fareDetails <&> (.trainTypeCode),
        ticketTypeCode = quote.fareDetails <&> (.ticketTypeCode)
      }

switchFRFSQuoteTierUtil ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  DJourneyLeg.JourneyLeg ->
  Id DFRFSQuote.FRFSQuote ->
  m ()
switchFRFSQuoteTierUtil journeyLeg quoteId = do
  whenJust journeyLeg.legSearchId $ \legSearchId -> do
    mbAlternateShortNames <- getAlternateRouteInfo
    let searchId = Id legSearchId
    QJourneyLeg.updateLegPricingIdByLegSearchId (Just quoteId.getId) journeyLeg.legSearchId
    mbBooking <- QFRFSTicketBooking.findBySearchId searchId
    whenJust mbBooking $ \booking -> do
      quote <- QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Quote not found")
      oldQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
      newQuoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quoteId
      quantitySyncedQuoteCategories <-
        mergeQuoteCategoriesWithQuantitySelections
          ( map
              ( \category -> (category.category, category.selectedQuantity)
              )
              oldQuoteCategories
          )
          newQuoteCategories
      let fareParameters = mkFareParameters (mkCategoryPriceItemFromQuoteCategories quantitySyncedQuoteCategories)
          totalPriceForSwitchLeg =
            Price
              { amount = fareParameters.totalPrice.amount,
                amountInt = fareParameters.totalPrice.amountInt,
                currency = fareParameters.currency
              }
          updatedBooking =
            booking
              { DFRFSTicketBooking.quoteId = quoteId,
                DFRFSTicketBooking.stationsJson = quote.stationsJson,
                DFRFSTicketBooking.routeStationsJson = quote.routeStationsJson,
                DFRFSTicketBooking.totalPrice = totalPriceForSwitchLeg
              }
      void $ QFRFSTicketBooking.updateByPrimaryKey updatedBooking
    whenJust mbAlternateShortNames $ \(alternateShortNames, alternateRouteIds) -> do
      QRouteDetails.updateAlternateShortNamesAndRouteIds alternateShortNames (Just alternateRouteIds) journeyLeg.id.getId
  where
    getAlternateRouteInfo ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        EncFlow m r,
        Monad m,
        HasField "ltsHedisEnv" r Hedis.HedisEnv,
        HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
        HasField "cloudType" r (Maybe CloudType),
        HasKafkaProducer r,
        HasShortDurationRetryCfg r c
      ) =>
      m (Maybe ([Text], [Text]))
    getAlternateRouteInfo = do
      options <- getLegTierOptionsUtil journeyLeg False
      let mbSelectedOption = find (\option -> option.quoteId == Just quoteId) options
      return $ mbSelectedOption <&> (\option -> unzip $ map (\a -> (a.shortName, a.routeCode)) option.availableRoutesInfo)

    mergeQuoteCategoriesWithQuantitySelections ::
      (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
      [(FRFSQuoteCategoryType, Int)] ->
      [DFRFSQuoteCategory.FRFSQuoteCategory] ->
      m [DFRFSQuoteCategory.FRFSQuoteCategory]
    mergeQuoteCategoriesWithQuantitySelections categories quoteCategories = do
      updatedQuoteCategories <- mapM updateCategory quoteCategories
      return updatedQuoteCategories
      where
        updateCategory category =
          case find (\(categoryType, _) -> categoryType == category.category) categories of
            Just (_, quantity) -> do
              QFRFSQuoteCategory.updateQuantityByQuoteCategoryId quantity category.id
              return category {DFRFSQuoteCategory.selectedQuantity = quantity}
            Nothing -> do
              QFRFSQuoteCategory.updateQuantityByQuoteCategoryId 0 category.id
              return category {DFRFSQuoteCategory.selectedQuantity = 0}

getLegTierOptionsUtil ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Hedis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasKafkaProducer r,
    HasShortDurationRetryCfg r c
  ) =>
  DJourneyLeg.JourneyLeg ->
  Bool ->
  m [DRouteDetails.AvailableRoutesByTier]
getLegTierOptionsUtil journeyLeg enableSuburbanRoundTrip = do
  now <- getCurrentTime
  let mbAgencyId = journeyLeg.agency >>= (.gtfsId)
  let vehicleCategory = castTravelModeToVehicleCategory journeyLeg.mode
  quotes <- maybe (pure []) (QFRFSQuote.findAllBySearchId . Id) journeyLeg.legSearchId
  availableServiceTiers <-
    mapMaybeM
      ( \quote -> do
          quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
          return $ getServiceTierFromQuote quoteCategories quote
      )
      quotes
  mbIntegratedBPPConfig <- SIBC.findMaybeIntegratedBPPConfigFromAgency mbAgencyId journeyLeg.merchantOperatingCityId vehicleCategory DIntegratedBPPConfig.MULTIMODAL
  let mbRouteDetail = journeyLeg.routeDetails & listToMaybe
  let mbFomStopCode = mbRouteDetail >>= (.fromStopCode)
  let mbToStopCode = mbRouteDetail >>= (.toStopCode)
  let mbArrivalTime = mbRouteDetail >>= (.fromArrivalTime)
  let arrivalTime = fromMaybe now mbArrivalTime

  case (mbFomStopCode, mbToStopCode, mbIntegratedBPPConfig) of
    (Just fromStopCode, Just toStopCode, Just integratedBPPConfig) -> do
      (_, availableRoutesByTiers, _) <- findPossibleRoutes (Just availableServiceTiers) fromStopCode toStopCode arrivalTime integratedBPPConfig journeyLeg.merchantId journeyLeg.merchantOperatingCityId vehicleCategory (vehicleCategory /= Enums.SUBWAY) False False enableSuburbanRoundTrip
      return availableRoutesByTiers
    _ -> return []

getLiveRouteInfo ::
  ( Metrics.HasBAPMetrics m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Text ->
  Text ->
  m (Maybe VehicleLiveRouteInfo)
getLiveRouteInfo integratedBPPConfig userPassedVehicleNumber userPassedRouteCode = do
  getLiveRouteInfo' integratedBPPConfig userPassedVehicleNumber (Just userPassedRouteCode)

getLiveRouteInfo' ::
  ( Metrics.HasBAPMetrics m r,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Text ->
  Maybe Text ->
  m (Maybe VehicleLiveRouteInfo)
getLiveRouteInfo' integratedBPPConfig userPassedVehicleNumber userPassedRouteCode = do
  fork "getVehicleLiveRouteInfo" $ Metrics.incrementBusScanSearchRequestCount "ANNA_APP" integratedBPPConfig.merchantOperatingCityId.getId
  mbVehicleOverrideInfo <- Dispatcher.getFleetOverrideInfo userPassedVehicleNumber
  mbRouteLiveInfo <-
    case mbVehicleOverrideInfo of
      Just (updatedVehicleNumber, newDeviceWaybillNo) -> do
        mbUpdatedVehicleRouteInfo <- getVehicleLiveRouteInfo [integratedBPPConfig] updatedVehicleNumber Nothing
        if Just newDeviceWaybillNo /= ((.waybillId) . snd =<< mbUpdatedVehicleRouteInfo)
          then do
            Dispatcher.delFleetOverrideInfo userPassedVehicleNumber
            getVehicleLiveRouteInfo [integratedBPPConfig] userPassedVehicleNumber Nothing
          else pure mbUpdatedVehicleRouteInfo
      Nothing -> getVehicleLiveRouteInfo [integratedBPPConfig] userPassedVehicleNumber Nothing
  return $
    maybe
      Nothing
      ( \routeLiveInfo@(VehicleLiveRouteInfo {..}) ->
          case userPassedRouteCode of
            Nothing -> Just routeLiveInfo
            Just _ ->
              if routeCode == userPassedRouteCode
                then Just routeLiveInfo
                else Just (VehicleLiveRouteInfo {routeCode = userPassedRouteCode, ..})
      )
      (snd <$> mbRouteLiveInfo)

getBlacklistedFilters ::
  Maybe Bool ->
  Maybe [Spec.ServiceTierType] ->
  ([Spec.ServiceTierType], [DFRFSQuote.FRFSQuoteType])
getBlacklistedFilters filterServiceAndJrnyType mbNewServiceTiers =
  let baseServiceTiers = [Spec.PREMIUM, Spec.SHUTTLE]

      -- flag based blacklist
      flagServiceTiers =
        if filterServiceAndJrnyType == Just False
          then []
          else [Spec.AC_EMU_FIRST_CLASS]

      flagFareQuoteTypes =
        if filterServiceAndJrnyType == Just False
          then []
          else [DFRFSQuote.ReturnJourney]

      -- list based blacklist
      -- mbNewServiceTiers controls how baseServiceTiers contribute to blacklist
      -- Nothing     -> blacklist all baseServiceTiers
      -- Just []     -> blacklist none
      -- Just xs     -> remove xs from baseServiceTiers and blacklist remaining
      listServiceTiers =
        case mbNewServiceTiers of
          Nothing -> baseServiceTiers
          Just [] -> []
          Just toKeep -> filter (`notElem` toKeep) baseServiceTiers

      blacklistedServiceTiers = flagServiceTiers ++ listServiceTiers
   in (blacklistedServiceTiers, flagFareQuoteTypes)

getRouteStopIndices ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasShortDurationRetryCfg r c, Log m, CacheFlow m r, EsqDBFlow m r) =>
  Text ->
  Text ->
  Text ->
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  m (Maybe (Int, Int))
getRouteStopIndices routeCode fromStationCode toStationCode integratedBppConfig = do
  IM.withInMemCache ["getRouteStopIndices", routeCode, fromStationCode, toStationCode, integratedBppConfig.id.getId] 86400 $ do
    routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBppConfig
    let sortedStops = sortOn (.sequenceNum) routeStops
        mFromIdx =
          findIndex (\s -> s.stopCode == fromStationCode) sortedStops
        mToIdx =
          findIndex (\s -> s.stopCode == toStationCode) sortedStops
    pure $ (,) <$> mFromIdx <*> mToIdx

getWaybillNoAndTripNoFromTripId :: T.Text -> (T.Text, Int)
getWaybillNoAndTripNoFromTripId tripId =
  case T.splitOn "-" tripId of
    [waybillNo, tripNoTxt] ->
      (waybillNo, fromMaybe 0 (readMaybe $ T.unpack tripNoTxt))
    _ -> (tripId, 0)

makeTripIdFromWaybillNoAndTripNo :: T.Text -> Int -> T.Text
makeTripIdFromWaybillNoAndTripNo waybillNo tripNo =
  waybillNo <> "-" <> T.pack (show tripNo)

meetsSeatQuota :: Int -> Int -> Seat.Seat -> Bool
meetsSeatQuota fromIdx toIdx seat =
  let numStops = toIdx - fromIdx
   in case seat.minStopsRequired of
        Just req -> numStops >= req
        Nothing -> True
