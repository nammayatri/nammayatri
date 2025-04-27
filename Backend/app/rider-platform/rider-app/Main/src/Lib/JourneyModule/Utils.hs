module Lib.JourneyModule.Utils where

import BecknV2.FRFS.Enums as Spec
import Control.Applicative ((<|>))
import Data.List (groupBy, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.IntegratedBPPConfig as DIntegratedBPPConfig
import Domain.Types.Journey
import Domain.Types.JourneyLeg
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MultimodalPreferences as DMP
import qualified Domain.Types.RecentLocation as DTRL
import Domain.Types.Route
import Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Trip as DTrip
import Domain.Utils (utctTimeToDayOfWeek)
import Kernel.External.MultiModal.Interface as MultiModal hiding (decode, encode)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.RouteStopTimeTable as GRSM
import qualified Storage.Queries.FRFSSearch as QFRFSearch
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.RecentLocation as SQRL
import qualified Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopCalender as QRouteCalendar
import qualified Storage.Queries.RouteStopMapping as QRouteStopMapping
import qualified Storage.Queries.RouteStopTimeTable as QRouteStopTiming
import qualified Storage.Queries.Station as QStation
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

data UpcomingBusInfo = UpcomingBusInfo
  { routeCode :: Text,
    serviceType :: Spec.ServiceTierType,
    arrivalTimeInSeconds :: Seconds
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | Data type representing upcoming trip information
data UpcomingTripInfo = UpcomingTripInfo
  { busFrequency :: Maybe Seconds,
    upcomingBuses :: [UpcomingBusInfo]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data LegServiceTier = LegServiceTier
  { serviceTierName :: Text,
    serviceTierType :: Spec.ServiceTierType,
    serviceTierDescription :: Text,
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
    quoteId :: Maybe (Id DFRFSQuote.FRFSQuote),
    availableRoutes :: [Text],
    nextAvailableBuses :: [Seconds],
    fare :: PriceAPIEntity
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

-- | Helper function to check which trips are serviceable on the current day
filterServiceableTrips ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    Monad m
  ) =>
  [Id RouteStopTimeTable] -> -- List of trip IDs to check
  UTCTime -> -- Current time
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  m [Id RouteStopTimeTable] -- List of serviceable trip IDs
filterServiceableTrips tripIds currentTime integratedBppConfigId = do
  routeCalendars <- QRouteCalendar.findByTripIds tripIds integratedBppConfigId
  let (_, currentTimeIST) = getISTTimeInfo currentTime
      today = fromEnum $ utctTimeToDayOfWeek currentTimeIST
      serviceableTrips = filter (\rc -> if length rc.serviceability > today then (rc.serviceability !! today) > 0 else False) routeCalendars
  return $ map (.tripId) serviceableTrips

-- | Find all possible routes from originStop to destinationStop with trips in the next hour
-- grouped by service tier type
findPossibleRoutes ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    Monad m
  ) =>
  Maybe [LegServiceTier] ->
  Text ->
  Text ->
  UTCTime ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m (Maybe Text, [AvailableRoutesByTier])
findPossibleRoutes mbAvailableServiceTiers fromStopCode toStopCode currentTime integratedBppConfigId mid mocid = do
  -- Get route mappings that contain the origin stop
  fromRouteStopMappings <- QRouteStopMapping.findByStopCode fromStopCode integratedBppConfigId

  -- Get route mappings that contain the destination stop
  toRouteStopMappings <- QRouteStopMapping.findByStopCode toStopCode integratedBppConfigId

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

  routeStopTimings <- measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfigId mid mocid validRoutes ("chennai_bus:" <> fromStopCode)) "fetch route stop timing through graphql"
  _ <- measureLatency (QRouteStopTiming.findByRouteCodeAndStopCode validRoutes fromStopCode integratedBppConfigId) "fetch route stop timing through sql"

  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime
      nextHourCutoff = addUTCTime 3600 currentTimeIST

  -- Get trip ids for calendar checking
  let tripIds = map (.tripId) routeStopTimings

  -- Check which trips are serviceable today
  let serviceableTripIds = tripIds

  -- Filter timings by serviceable trips and future arrivals within the next hour
  let validTimings =
        [ timing
          | timing <- routeStopTimings,
            timing.tripId `elem` serviceableTripIds,
            let arrivalTime = getISTArrivalTime timing.timeOfArrival currentTime,
            arrivalTime > currentTimeIST && arrivalTime <= nextHourCutoff
        ]

  let sortedTimings =
        sortBy
          ( \a b ->
              let aTime = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime a.timeOfArrival currentTime) currentTimeIST
                  bTime = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime b.timeOfArrival currentTime) currentTimeIST
               in compare aTime bTime
          )
          validTimings

  -- Group by service tier
  let groupedByTier = groupBy (\a b -> a.serviceTierType == b.serviceTierType) $ sortBy (comparing (.serviceTierType)) validTimings

  -- For each service tier, collect route information
  results <- forM groupedByTier $ \timingsForTier -> do
    let serviceTierType = if null timingsForTier then Spec.ORDINARY else (head timingsForTier).serviceTierType
        routeCodesForTier = nub $ map (.routeCode) timingsForTier

    -- Get route details to include the short name
    routeDetails <- mapM (\routeCode -> QRoute.findByRouteCode routeCode integratedBppConfigId) routeCodesForTier
    let validRouteDetails = catMaybes routeDetails
        routeShortNames = nub $ map (.shortName) validRouteDetails

    -- Calculate arrival times in seconds
    let arrivalTimes =
          [ nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime timing.timeOfArrival currentTime) currentTimeIST
            | timing <- timingsForTier
          ]

    logDebug $ "routeShortNames: " <> show routeShortNames

    (mbFare, serviceTierName, serviceTierDescription, quoteId) <- do
      case mbAvailableServiceTiers of
        Just availableServiceTiers -> do
          let availableServiceTier = find (\tier -> tier.serviceTierType == serviceTierType) availableServiceTiers
              quoteId = availableServiceTier <&> (.quoteId)
              serviceTierName = availableServiceTier <&> (.serviceTierName)
              serviceTierDescription = availableServiceTier <&> (.serviceTierDescription)
              mbFare = availableServiceTier <&> (.fare)
          return (mbFare, serviceTierName, serviceTierDescription, quoteId)
        Nothing -> return (Nothing, Nothing, Nothing, Nothing)

    logDebug $ "mbFare: " <> show mbFare
    let fare = fromMaybe (PriceAPIEntity 0.0 INR) mbFare -- fix it later
    return $
      AvailableRoutesByTier
        { serviceTier = serviceTierType,
          availableRoutes = routeShortNames,
          nextAvailableBuses = sort arrivalTimes,
          serviceTierName = serviceTierName,
          serviceTierDescription = serviceTierDescription,
          quoteId = quoteId,
          fare = fare
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
    Monad m
  ) =>
  [Text] ->
  Text ->
  Maybe Spec.ServiceTierType ->
  UTCTime ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m UpcomingTripInfo
findUpcomingTrips routeCodes stopCode mbServiceType currentTime integratedBppConfigId mid mocid = do
  -- Get IST time info
  let (_, currentTimeIST) = getISTTimeInfo currentTime

  routeStopTimings <- measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfigId mid mocid routeCodes ("chennai_bus:" <> stopCode)) "fetch route stop timing through graphql"
  _ <- measureLatency (QRouteStopTiming.findByRouteCodeAndStopCode routeCodes stopCode integratedBppConfigId) "fetch route stop timing through sql"

  let filteredByService = case mbServiceType of
        Just serviceType -> filter (\rst -> rst.serviceTierType == serviceType) routeStopTimings
        Nothing -> routeStopTimings

  let tripIds = map (.tripId) filteredByService

  -- Check which trips are serviceable today
  let serviceableTripIds = tripIds

  -- Combine stop timings with their calendars and get arrival times
  -- Filter out trips that have already passed the stop
  let tripTimingsWithCalendars =
        [ UpcomingBusInfo
            { routeCode = rst.routeCode,
              serviceType = rst.serviceTierType,
              arrivalTimeInSeconds = nominalDiffTimeToSeconds $ diffUTCTime (getISTArrivalTime rst.timeOfArrival currentTime) currentTimeIST
            }
          | rst <- filteredByService,
            rst.tripId `elem` serviceableTripIds,
            (getISTArrivalTime rst.timeOfArrival currentTime) > currentTimeIST -- Only include future arrivals
        ]

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
            upcomingBuses = upcomingBuses
          }
  return upcomingTripInfo

data StopDetails = StopDetails
  { stopCode :: Text,
    stopName :: Text,
    stopLat :: Double,
    stopLon :: Double,
    stopArrivalTime :: UTCTime
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
    Monad m
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Id DIntegratedBPPConfig.IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  m (Maybe SingleModeRouteDetails)
getSingleModeRouteDetails mbRouteCode (Just originStopCode) (Just destinationStopCode) integratedBppConfigId mid mocid = do
  mbFromStop <- QStation.findByStationCode originStopCode integratedBppConfigId
  mbToStop <- QStation.findByStationCode destinationStopCode integratedBppConfigId
  currentTime <- getCurrentTime

  case (mbFromStop, mbToStop) of
    (Just fromStop, Just toStop) -> do
      case (fromStop.lat, fromStop.lon, toStop.lat, toStop.lon) of
        (Just fromStopLat, Just fromStopLon, Just toStopLat, Just toStopLon) -> do
          (nextAvailableRouteCode, possibleRoutes) <- measureLatency (findPossibleRoutes Nothing originStopCode destinationStopCode currentTime integratedBppConfigId mid mocid) "findPossibleRoutes"

          let routeCode = mbRouteCode <|> nextAvailableRouteCode
          mbRoute <- maybe (return Nothing) (\rc -> QRoute.findByRouteCode rc integratedBppConfigId) routeCode
          case mbRoute of
            Just route -> do
              -- Get timing information for this route at the origin stop
              originStopTimings <- measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfigId mid mocid [route.code] ("chennai_bus:" <> originStopCode)) "route stop timing through graphql"
              _ <- measureLatency (QRouteStopTiming.findByRouteCodeAndStopCode [route.code] originStopCode integratedBppConfigId) "route stop timing through sql"
              destStopTimings <- measureLatency (GRSM.findByRouteCodeAndStopCode integratedBppConfigId mid mocid [route.code] ("chennai_bus:" <> originStopCode)) "route stop timing through graphql"
              _ <- measureLatency (QRouteStopTiming.findByRouteCodeAndStopCode [route.code] destinationStopCode integratedBppConfigId) "route stop timing through sql"
              -- Get trip IDs for calendar checking
              let serviceableTripIds = map (.tripId) originStopTimings

              -- Get IST time info
              let (_, currentTimeIST) = getISTTimeInfo currentTime

              -- Find the earliest upcoming departure from origin stop
              let validOriginTimings =
                    [ timing
                      | timing <- originStopTimings,
                        timing.tripId `elem` serviceableTripIds,
                        let departureTime = getISTArrivalTime timing.timeOfDeparture currentTime,
                        departureTime > currentTimeIST
                    ]

              let mbEarliestOriginTiming =
                    listToMaybe $
                      sortBy
                        ( \a b ->
                            compare
                              (getISTArrivalTime a.timeOfDeparture currentTime)
                              (getISTArrivalTime b.timeOfDeparture currentTime)
                        )
                        validOriginTimings

              -- Find the corresponding destination arrival for the same trip
              let mbDestinationTiming = do
                    originTiming <- mbEarliestOriginTiming
                    find (\dt -> dt.tripId == originTiming.tripId) destStopTimings

              -- Create stop details
              let mbDepartureTime = getISTArrivalTime . (.timeOfDeparture) <$> mbEarliestOriginTiming <*> pure currentTime
                  mbArrivalTime = getISTArrivalTime . (.timeOfArrival) <$> mbDestinationTiming <*> pure currentTime
                  fromStopDetails = StopDetails fromStop.code fromStop.name fromStopLat fromStopLon (fromMaybe currentTime mbDepartureTime)
                  toStopDetails = StopDetails toStop.code toStop.name toStopLat toStopLon (fromMaybe currentTime mbArrivalTime)

              return $ Just $ SingleModeRouteDetails fromStopDetails toStopDetails route (concatMap (.availableRoutes) possibleRoutes)
            Nothing -> return Nothing
        _ -> return Nothing
    _ -> return Nothing
getSingleModeRouteDetails _ _ _ _ _ _ = return Nothing

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
  mbRecentLocationId <- getRecentLocationId onlyPublicTransportLegs
  case mbRecentLocationId of
    Just recentLocationId -> SQRL.increaceFrequencyById recentLocationId
    Nothing -> do
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
          let recentLocation =
                DTRL.RecentLocation
                  { id = uuid,
                    riderId = journey.riderId,
                    frequency = 1,
                    entityType = if length legs > 1 then DTRL.MULTIMODAL else convertModeToEntityType firstLeg.mode,
                    address = Nothing,
                    fromLatLong = Just $ LatLong firstLeg.startLocation.latitude firstLeg.startLocation.longitude,
                    routeCode = if length legs > 1 then Nothing else mbRouteCode,
                    toStopCode = Just lastStopCode,
                    fromStopCode = Just firstStopCode,
                    toLatLong = LatLong endLocation.latitude endLocation.longitude,
                    merchantOperatingCityId = cityId,
                    createdAt = now,
                    updatedAt = now
                  }
          SQRL.create recentLocation
        _ -> return ()
  where
    getRecentLocationId :: (MonadFlow m, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => [JourneyLeg] -> m (Maybe (Id DTRL.RecentLocation))
    getRecentLocationId legs = do
      mbFrfsSearch <- maybe (pure Nothing) (QFRFSearch.findById . Id) ((listToMaybe legs) >>= (.legSearchId))
      return $ mbFrfsSearch >>= (.recentLocationId)
