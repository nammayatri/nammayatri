module Lib.JourneyModule.Utils where

import qualified Beckn.OnDemand.Utils.Common as UCommon
import BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Geohash as Geohash
import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, nub, sort, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalPreferences as DMP
import Domain.Types.Person
import qualified Domain.Types.RecentLocation as DTRL
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Trip as DTrip
import Environment
import ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
import Kernel.External.Encryption
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import qualified Kernel.External.Payment.Interface.Types as KT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified SharedLogic.CreateFareForMultiModal as SMMFRFS
import SharedLogic.FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant.MultiModalBus as MultiModalBus
import qualified Storage.CachedQueries.Merchant.MultiModalSuburban as MultiModalSuburban
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.RouteStopTimeTable as GRSM
import Storage.GraphqlQueries.Client (mapToServiceTierType)
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RecentLocation as SQRL
import qualified Storage.Queries.VehicleRouteMapping as QVehicleRouteMapping
import qualified System.Environment as Se
import Tools.Maps (LatLong (..))
import qualified Tools.Maps as Maps
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

data AvailableRoutesInfo = AvailableRoutesInfo
  { shortName :: Text,
    routeCode :: Text,
    isLiveTrackingAvailable :: Bool
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data JourneyLegOption = JourneyLegOption
  { viaPoints :: [Text],
    routeDetails :: [JourneyLegRouteDetails],
    arrivalTimes :: [Seconds],
    availableRoutes :: [Text],
    fare :: HighPrecMoney,
    duration :: Maybe Seconds,
    distance :: Maybe Distance,
    journeyLegId :: Id DJourneyLeg.JourneyLeg
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data JourneyLegRouteDetails = JourneyLegRouteDetails
  { fromStopCode :: Text,
    toStopCode :: Text,
    subLegOrder :: Int
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Data structure to represent available routes grouped by service tier
data AvailableRoutesByTier = AvailableRoutesByTier
  { serviceTier :: Spec.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierDescription :: Maybe Text,
    via :: Maybe Text,
    trainTypeCode :: Maybe Text,
    quoteId :: Maybe (Id DFRFSQuote.FRFSQuote),
    availableRoutes :: [Text], -- todo: deprecate this
    availableRoutesInfo :: [AvailableRoutesInfo],
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
  disableLiveBuses <- fromMaybe False . (>>= readMaybe) <$> (liftIO $ Se.lookupEnv "DISABLE_LIVE_BUSES")
  if not disableLiveBuses
    then do
      allRouteWithBuses <- MultiModalBus.getBusesForRoutes routeCodes
      routeStopTimes <- mapM processRoute allRouteWithBuses
      let flattenedRouteStopTimes = concat routeStopTimes
      logDebug $ "allRouteWithBuses: " <> show allRouteWithBuses <> " routeStopTimes: " <> show routeStopTimes <> " flattenedRouteStopTimes: " <> show flattenedRouteStopTimes <> " disableLiveBuses: " <> show disableLiveBuses
      if not (null flattenedRouteStopTimes)
        then return flattenedRouteStopTimes
        else measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode) "fetch route stop timing through graphql"
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
      serviceTierToShortNameMapping :: HM.HashMap Text (Maybe Text) <- -- HashMap SeriveTier ServiceTierNameForUI
        HM.fromList
          <$> mapM
            ( \serviceTierTypeString -> do
                let serviceTierType = mapToServiceTierType serviceTierTypeString
                frfsServiceTier <- CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityId serviceTierType mocid
                return (serviceTierTypeString, frfsServiceTier <&> (.shortName))
            )
            (map (\(_, mapping) -> mapping) validBuses)
      let baseStopTimes = map (createStopTime serviceTierToShortNameMapping) validBuses
      return baseStopTimes
      where
        createStopTime serviceTierToShortNameMapping ((vehicleNumber, eta), mapping) = createRouteStopTimeTable serviceTierToShortNameMapping routeWithBuses vehicleNumber eta mapping

    createRouteStopTimeTable serviceTierToShortNameMapping routeWithBuses vehicleNumber eta mapping =
      let timeOfDay = timeToTimeOfDay $ utctDayTime eta.arrivalTime
          serviceTierName = fromMaybe Nothing $ HM.lookup mapping serviceTierToShortNameMapping
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
              serviceTierName = serviceTierName,
              delay = Nothing,
              source = LIVE,
              stage = Nothing,
              platformCode = Nothing,
              providerStopCode = Nothing,
              isStageStop = Nothing
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

-- Function for explicitly fetching static timings for buses to give more possibleRoutes for buses
fetchStaticTimings ::
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
  DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m [RouteStopTimeTable]
fetchStaticTimings routeCodes stopCode integratedBppConfig mid mocid =
  measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfig mid mocid routeCodes stopCode) ("fetchStaticTimings" <> show routeCodes <> " fromStopCode: " <> show stopCode <> " fromStopCode: " <> show stopCode)

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
  m (Maybe Text, [AvailableRoutesByTier])
findPossibleRoutes mbAvailableServiceTiers fromStopCode toStopCode currentTime integratedBppConfig mid mocid vc sendWithoutFare = do
  -- Get route mappings that contain the origin stop
  fromRouteStopMappings <- measureLatency (OTPRest.getRouteStopMappingByStopCode fromStopCode integratedBppConfig) ("getRouteStopMappingByStopCode" <> show fromStopCode)

  -- Get route mappings that contain the destination stop
  toRouteStopMappings <- measureLatency (OTPRest.getRouteStopMappingByStopCode toStopCode integratedBppConfig) ("getRouteStopMappingByStopCode" <> show toStopCode)

  -- Find common routes that have both the origin and destination stops
  -- and ensure that from-stop comes before to-stop in the route sequence
  let fromRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) fromRouteStopMappings
      toRouteStopMap = map (\mapping -> (mapping.routeCode, mapping.sequenceNum)) toRouteStopMappings
      validRoutes =
        nub $
          [ fromRouteCode
            | (fromRouteCode, fromSeq) <- fromRouteStopMap,
              (toRouteCode, toSeq) <- toRouteStopMap,
              fromRouteCode == toRouteCode && fromSeq < toSeq -- Ensure correct sequence
          ]

  liveBusesInfo <- mapM (\routeInfo -> (routeInfo,) <$> MultiModalBus.getRoutesBuses routeInfo) validRoutes

  busRouteMapping :: HM.HashMap RouteCodeText (HM.HashMap ServiceTypeText Text) <-
    HM.fromList <$> forM liveBusesInfo \(routeInfo, routeWithBuses) -> do
      busMappings <-
        mapMaybeM
          ( \bus -> do
              mbServiceType <- OTPRest.getVehicleServiceType integratedBppConfig bus.vehicleNumber
              return $ (\serviceTypeResp -> (show serviceTypeResp.service_type, bus.vehicleNumber)) <$> mbServiceType
          )
          routeWithBuses.buses
      return (routeInfo, HM.fromList busMappings)
  -- Get the timing information for these routes at the origin stop

  -- We get static timings in this call if live timings are not found
  routeStopTimingsLive <- measureLatency (fetchLiveTimings validRoutes fromStopCode currentTime integratedBppConfig mid mocid vc) ("fetchLiveTimings" <> show validRoutes <> " fromStopCode: " <> show fromStopCode <> " toStopCode: " <> show toStopCode)
  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  let fetchLiveTimingsSource = fromMaybe GTFS $ listToMaybe $ map (.source) routeStopTimingsLive
  -- Fetch static timings for buses if not already fetched by `fetchLiveTimings`
  routeStopTimingsStatic <- case fetchLiveTimingsSource of
    GTFS -> pure [] -- Already timings are from static source
    _ -> case vc of
      Enums.BUS -> fetchStaticTimings validRoutes fromStopCode integratedBppConfig mid mocid
      _ -> pure []
  let liveRouteCodes = Set.fromList [rst.routeCode | rst <- routeStopTimingsLive]
  let routeStopTimings =
        routeStopTimingsLive
          <> [s | s <- routeStopTimingsStatic, s.routeCode `Set.notMember` liveRouteCodes]

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
    let tierSource = fromMaybe GTFS $ listToMaybe $ map (.source) timingsForTier

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
    let availableRoutesInfo =
          map
            ( \routeDetail -> do
                let busForCurrentServiceType = HM.lookup (show serviceTierType) =<< HM.lookup routeDetail.code busRouteMapping
                AvailableRoutesInfo {shortName = routeDetail.shortName, routeCode = routeDetail.code, isLiveTrackingAvailable = isJust busForCurrentServiceType}
            )
            validRouteDetails
    return $
      AvailableRoutesByTier
        { serviceTier = serviceTierType,
          availableRoutes = routeShortNames, -- TODO: deprecate this
          availableRoutesInfo = availableRoutesInfo,
          nextAvailableBuses = sort arrivalTimes,
          serviceTierName = serviceTierName,
          serviceTierDescription = serviceTierDescription,
          quoteId = quoteId,
          via = via,
          trainTypeCode = trainTypeCode,
          fare = fare,
          nextAvailableTimings = sortBy (\a b -> compare (fst a) (fst b)) nextAvailableTimings,
          source = tierSource
        }

  -- Only return service tiers that have available routes
  return $ ((listToMaybe sortedTimings) <&> (.routeCode), filter (\r -> not (null $ r.availableRoutes) && (r.fare /= PriceAPIEntity 0.0 INR || sendWithoutFare)) results)

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
              serviceName = rst.serviceTierName,
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

measureLatency :: MonadFlow m => m a -> Text -> m a
measureLatency action label = do
  startTime <- getCurrentTime
  result <- action
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

mkMultiModalRoute :: UTCTime -> Maybe MultiModalTypes.MultiModalLeg -> MultiModalTypes.GeneralVehicleType -> NonEmpty MultiModalTypes.MultiModalRouteDetails -> MultiModalTypes.MultiModalRoute
mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode routeDetails = do
  let firstRouteDetails = NonEmpty.head routeDetails
  let lastRouteDetails = NonEmpty.last routeDetails
  let duration = nominalDiffTimeToSeconds $ diffUTCTime (fromMaybe currentTimeIST lastRouteDetails.toArrivalTime) (fromMaybe currentTimeIST firstRouteDetails.fromArrivalTime)
  let distance = convertHighPrecMetersToDistance Meter (distanceBetweenInMeters (locationV2ToLatLong firstRouteDetails.startLocation) (locationV2ToLatLong lastRouteDetails.endLocation))
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
            exit = Nothing
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

buildMultimodalRouteDetails ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m,
    HasField "ltsHedisEnv" r Hedis.HedisEnv,
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
  m (Maybe MultiModalTypes.MultiModalRouteDetails, Maybe HighPrecMeters)
buildMultimodalRouteDetails subLegOrder mbRouteCode originStopCode destinationStopCode integratedBppConfig mid mocid vc = do
  mbFromStop <- OTPRest.getStationByGtfsIdAndStopCode originStopCode integratedBppConfig
  mbToStop <- OTPRest.getStationByGtfsIdAndStopCode destinationStopCode integratedBppConfig
  currentTime <- getCurrentTime
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  case (mbFromStop >>= (.lat), mbFromStop >>= (.lon), mbToStop >>= (.lat), mbToStop >>= (.lon)) of
    (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
      (nextAvailableRouteCode, possibleRoutes) <- measureLatency (findPossibleRoutes Nothing originStopCode destinationStopCode currentTime integratedBppConfig mid mocid vc True) "findPossibleRoutes"
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
          originStopTimings <- fetchLiveTimings [route.code] originStopCode currentTime integratedBppConfig mid mocid vc
          destStopTimings <- fetchLiveTimings [route.code] destinationStopCode currentTime integratedBppConfig mid mocid vc

          let stopCodeToSequenceNum = Map.fromList $ map (\rst -> (rst.stopCode, rst.sequenceNum)) routeStopMappings

          let mbEarliestOriginTiming =
                findEarliestTiming currentTimeIST currentTime $
                  sortBy
                    ( \a b ->
                        compare
                          (getISTArrivalTime a.timeOfDeparture currentTime)
                          (getISTArrivalTime b.timeOfDeparture currentTime)
                    )
                    originStopTimings

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
  (LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)) ->
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
  (mbRouteDetails, _) <- buildMultimodalRouteDetails 1 mbRouteCode originStopCode destinationStopCode integratedBppConfig mid mocid vc
  case mbRouteDetails of
    Just routeDetails -> do
      currentTime <- getCurrentTime
      let (_, currentTimeIST) = getISTTimeInfo currentTime
      mbPreliminaryLeg <- getPreliminaryLeg routeDetails.startLocation
      return [mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode (NonEmpty.fromList [routeDetails])]
    Nothing -> return []
buildSingleModeDirectRoutes _ _ _ _ _ _ _ _ _ = return []

buildTrainAllViaRoutes ::
  (LocationV2 -> Flow (Maybe MultiModalTypes.MultiModalLeg)) ->
  Maybe Text ->
  Maybe Text ->
  Maybe DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Id Person ->
  Enums.VehicleCategory ->
  MultiModalTypes.GeneralVehicleType ->
  Flow [MultiModalTypes.MultiModalRoute]
buildTrainAllViaRoutes getPreliminaryLeg (Just originStopCode) (Just destinationStopCode) (Just integratedBppConfig) mid mocid personId vc mode = do
  allSubwayRoutes <- getAllSubwayRoutes
  mapMaybeM
    ( \viaRoutes -> do
        -- consider distance for future
        routeDetailsWithDistance <- mapM (\(osc, dsc) -> buildMultimodalRouteDetails 1 Nothing osc dsc integratedBppConfig mid mocid vc) viaRoutes
        logDebug $ "buildTrainAllViaRoutes routeDetailsWithDistance: " <> show routeDetailsWithDistance
        -- ensure that atleast one train route is possible or two stops are less than 1km apart so that user can walk to other station e.g. Chennai Park to Central station
        let isRoutePossible = all (\(mbRouteDetails, mbDistance) -> isJust mbRouteDetails || (isJust mbDistance && mbDistance < Just (HighPrecMeters 1000))) routeDetailsWithDistance
        if isRoutePossible
          then do
            let routeDetails = mapMaybe (\(mbRouteDetails, _) -> mbRouteDetails) routeDetailsWithDistance
            logDebug $ "buildTrainAllViaRoutes routeDetails: " <> show routeDetails
            let updateRouteDetails = zipWith (\idx routeDetail -> routeDetail {MultiModalTypes.subLegOrder = idx}) [1 ..] routeDetails
            case updateRouteDetails of
              [] -> return Nothing
              (rD : _) -> do
                currentTime <- getCurrentTime
                let (_, currentTimeIST) = getISTTimeInfo currentTime
                mbPreliminaryLeg <- getPreliminaryLeg rD.startLocation
                return $ Just $ mkMultiModalRoute currentTimeIST mbPreliminaryLeg mode (NonEmpty.fromList updateRouteDetails)
          else return Nothing
    )
    allSubwayRoutes
  where
    getAllSubwayRoutes :: Flow [[(Text, Text)]]
    getAllSubwayRoutes = do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbMobileNumber <- mapM decrypt person.mobileNumber
      mbImeiNumber <- mapM decrypt person.imeiNumber
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      case integratedBppConfig.providerConfig of
        DIntegratedBPPConfig.CRIS crisConfig -> do
          let routeFareReq =
                CRISRouteFare.CRISFareRequest
                  { mobileNo = mbMobileNumber,
                    imeiNo = fromMaybe "ed409d8d764c04f7" mbImeiNumber,
                    appSession = sessionId,
                    sourceCode = originStopCode,
                    changeOver = " ",
                    destCode = destinationStopCode,
                    via = " "
                  }
          fares <- CRISRouteFare.getRouteFare crisConfig mocid routeFareReq
          let viaPoints =
                nub $
                  map
                    ( \fd ->
                        let viaStops = if T.strip fd.via == "" then [] else T.splitOn "-" (T.strip fd.via)
                            stops = [originStopCode] <> viaStops <> [destinationStopCode]
                         in zipWith (,) stops (drop 1 stops)
                    )
                    $ mapMaybe (.fareDetails) fares
          logDebug $ "getAllSubwayRoutes viaPoints: " <> show viaPoints
          return viaPoints
        _ -> return []
buildTrainAllViaRoutes _ _ _ _ _ _ _ _ _ = return []

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
  let fare = foldl' (\acc leg -> acc + fromMaybe 0 leg.estimatedMinFare) 0 onlyPublicTransportLegs
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

postMultimodalPaymentUpdateOrderUtil :: (ServiceFlow m r, EncFlow m r, EsqDBReplicaFlow m r, HasField "isMetroTestTransaction" r Bool) => TPayment.PaymentServiceType -> Person -> Id Merchant -> Id MerchantOperatingCity -> [DFRFSTicketBooking.FRFSTicketBooking] -> m (Maybe DOrder.PaymentOrder)
postMultimodalPaymentUpdateOrderUtil paymentType person merchantId merchantOperatingCityId bookings = do
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow person.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show person.merchantOperatingCityId)
  frfsBookingsPayments <- mapMaybeM (QFRFSTicketBookingPayment.findNewTBPByBookingId . (.id)) bookings
  (vendorSplitDetails, amountUpdated) <- SMMFRFS.createVendorSplitFromBookings bookings merchantId person.merchantOperatingCityId paymentType frfsConfig.isFRFSTestingEnabled
  isSplitEnabled <- TPayment.getIsSplitEnabled merchantId person.merchantOperatingCityId Nothing paymentType -- TODO :: You can be moved inside :)
  let splitDetails = TPayment.mkUnaggregatedSplitSettlementDetails isSplitEnabled amountUpdated vendorSplitDetails
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
                    splitSettlementDetails = splitDetails
                  }
          _ <- TPayment.updateOrder person.merchantId person.merchantOperatingCityId Nothing TPayment.FRFSMultiModalBooking (Just person.id.getId) person.clientSdkVersion updateReq
          QOrder.updateAmount paymentOrder.id amountUpdated
          let updatedOrder :: DOrder.PaymentOrder
              updatedOrder = paymentOrder {DOrder.amount = amountUpdated}
          return $ Just updatedOrder
    else createPaymentOrder bookings merchantOperatingCityId merchantId amountUpdated person paymentType vendorSplitDetails

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
    try @_ @SomeException $
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
