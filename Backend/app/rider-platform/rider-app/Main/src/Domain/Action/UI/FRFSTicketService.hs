module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import qualified API.Types.UI.MultimodalConfirm as MultimodalConfirm
import BecknV2.FRFS.Enums hiding (END, START)
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import qualified Data.HashMap.Strict as HashMap
import Data.List (groupBy, nub, nubBy)
import qualified Data.List.NonEmpty as NonEmpty hiding (groupBy, map, nub, nubBy)
import Data.List.Split (chunksOf)
import qualified Data.Time as Time
import qualified Domain.Action.Beckn.FRFS.Common as FRFSCommon
import qualified Domain.Action.Beckn.FRFS.OnConfirm as DACFOC
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSQuoteCategory as FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingFeedback as DFRFSTicketBookingFeedback
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified Environment
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, forM_, fromList, groupBy, id, length, map, mapM_, maximum, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Maps.Types
import Kernel.External.MultiModal.Utils
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (whenJust)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.External.Nandi.Types (StopInfo (..), StopSchedule (..))
import SharedLogic.FRFSUtils
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.FRFSUtils as Utils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler as JobScheduler
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.FRFSConfig as CQFRFSConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSQuoteCategory as QFRFSQuoteCategory
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingFeedback as QFRFSTicketBookingFeedback
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RouteDetails as QRouteDetails
import Tools.Error
import Tools.Maps as Maps
import Tools.Metrics.BAPMetrics (HasBAPMetrics)
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

getFrfsRoutes ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Maybe Text ->
  Maybe Text ->
  Context.City ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
getFrfsRoutes (_personId, _mId) mbEndStationCode mbStartStationCode _city _vehicleType = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity _mId _city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _mId.getId <> "-city-" <> show _city)
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory _vehicleType) DIBC.APPLICATION
  SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig ->
    case (mbStartStationCode, mbEndStationCode) of
      (Just startStationCode, Just endStationCode) -> do
        routesInfo <- getPossibleRoutesBetweenTwoStops startStationCode endStationCode integratedBPPConfig
        return $
          map
            ( \routeInfo ->
                let stops =
                      ( mapWithIndex
                          ( \idx stop ->
                              FRFSStationAPI
                                { name = Just stop.stopName,
                                  code = stop.stopCode,
                                  routeCodes = Nothing,
                                  lat = Just stop.stopPoint.lat,
                                  lon = Just stop.stopPoint.lon,
                                  stationType = Just (if idx == 0 then START else if maybe False (\stops' -> idx < length stops') routeInfo.stops then INTERMEDIATE else END),
                                  sequenceNum = Just stop.sequenceNum,
                                  address = Nothing,
                                  timeTakenToTravelUpcomingStop = Nothing,
                                  distance = Nothing,
                                  color = Nothing,
                                  towards = Nothing,
                                  integratedBppConfigId = integratedBPPConfig.id,
                                  parentStopCode = Nothing
                                }
                          )
                      )
                        <$> routeInfo.stops
                 in FRFSTicketService.FRFSRouteAPI
                      { code = routeInfo.route.code,
                        shortName = routeInfo.route.shortName,
                        longName = routeInfo.route.longName,
                        startPoint = routeInfo.route.startPoint,
                        endPoint = routeInfo.route.endPoint,
                        totalStops = routeInfo.totalStops,
                        stops = stops,
                        timeBounds = Just routeInfo.route.timeBounds,
                        waypoints = Nothing,
                        integratedBppConfigId = integratedBPPConfig.id
                      }
            )
            routesInfo
      _ -> do
        routes <- OTPRest.getRoutesByVehicleType integratedBPPConfig _vehicleType
        return $
          map
            ( \Route.Route {..} -> FRFSTicketService.FRFSRouteAPI {totalStops = Nothing, stops = Nothing, waypoints = Nothing, timeBounds = Nothing, integratedBppConfigId = integratedBPPConfig.id, ..}
            )
            routes
  where
    mapWithIndex f xs = zipWith f [0 ..] xs

data StationResult = StationResult
  { code :: Kernel.Prelude.Text,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCodes :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    lat :: Double,
    lon :: Double,
    stationType :: Maybe StationType,
    sequenceNum :: Maybe Int,
    parentStopCode :: Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON)

instance HasCoordinates StationResult where
  getCoordinates stop = LatLong (stop.lat) (stop.lon)

getFrfsRoute ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Text ->
  Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) ->
  Maybe DIBC.PlatformType ->
  Context.City ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  Environment.Flow API.Types.UI.FRFSTicketService.FRFSRouteAPI
getFrfsRoute (_personId, _mId) routeCode mbIntegratedBPPConfigId _platformType _mbCity vehicleType = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity _mId _mbCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> _mId.getId <> "-city-" <> show _mbCity)
  let platformType = fromMaybe DIBC.APPLICATION _platformType
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) platformType
  route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
  routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
      firstStop = listToMaybe stopsSortedBySequenceNumber
  stops <-
    if isJust firstStop
      then do
        -- Use the new getExampleTrip API to get trip details directly
        tripDetails <- OTPRest.getExampleTrip integratedBPPConfig route.code
        case tripDetails of
          Just tripInfo -> do
            -- Convert TripStopDetail to the format expected by the existing logic
            let tripStops = tripInfo.stops
                -- Create schedule-like data from TripStopDetail
                stopSchedules = map (\stop -> StopSchedule stop.stopCode stop.scheduledArrival stop.scheduledDeparture stop.stopPosition) tripStops
                -- Create stop info-like data from TripStopDetail using the new fields
                stopInfos = map (\stop -> StopInfo stop.stopId stop.stopCode (fromMaybe stop.stopCode stop.stopName) stop.stopPosition stop.lat stop.lon) tripStops
                hashmapSchedule = HashMap.fromList $ map (\stop -> (stop.stopCode, stop)) stopSchedules
                hashmapStop = HashMap.fromList $ map (\stop -> (stop.stopCode, stop)) stopInfos
            foldM
              ( \processedStops stop -> do
                  let stopSchedule = HashMap.lookup stop.stopCode hashmapSchedule
                      stopInfo = HashMap.lookup stop.stopCode hashmapStop
                  let (_, timeTakenToTravelUpcomingStop) =
                        case processedStops of
                          (nextStopSchedule, _) : _ ->
                            -- Calculate time from current stop to next stop
                            case (stopSchedule, nextStopSchedule) of
                              (Just currentSchedule, Just nextSchedule) ->
                                let currentDepartureTime = secondsToTimeOfDay' currentSchedule.arrivalTime
                                    nextArrivalTime = secondsToTimeOfDay' nextSchedule.arrivalTime
                                 in (stopSchedule, Just $ diffTimeOfDay currentDepartureTime nextArrivalTime)
                              _ -> (stopSchedule, Nothing)
                          [] -> (stopSchedule, Just 0) -- Last stop (processed in reverse)
                  case stopInfo of
                    Just info ->
                      return $
                        ( stopSchedule,
                          FRFSStationAPI
                            { name = Just info.stopName,
                              code = info.stopCode,
                              routeCodes = Just [route.code],
                              lat = Just info.lat,
                              lon = Just info.lon,
                              timeTakenToTravelUpcomingStop = Seconds <$> timeTakenToTravelUpcomingStop,
                              stationType = Nothing,
                              sequenceNum = Just info.sequenceNum,
                              address = Nothing,
                              distance = Nothing,
                              color = Nothing,
                              towards = Nothing,
                              integratedBppConfigId = integratedBPPConfig.id,
                              parentStopCode = Nothing
                            }
                        ) :
                        processedStops
                    Nothing -> return processedStops
              )
              []
              (reverse stopsSortedBySequenceNumber)
          Nothing -> return []
      else return []

  return $
    FRFSTicketService.FRFSRouteAPI
      { code = route.code,
        shortName = route.shortName,
        longName = route.longName,
        startPoint = route.startPoint,
        endPoint = route.endPoint,
        totalStops = Just $ length stops,
        stops = Just $ map snd stops,
        timeBounds = Just route.timeBounds,
        waypoints = route.polyline <&> decode <&> fmap (\point -> LatLong {lat = point.latitude, lon = point.longitude}),
        integratedBppConfigId = integratedBPPConfig.id
      }
  where
    -- utcToTimeOfDay :: UTCTime -> TimeOfDay
    -- utcToTimeOfDay = Time.timeToTimeOfDay . Time.utctDayTime

    secondsToTimeOfDay' :: Int -> TimeOfDay
    secondsToTimeOfDay' seconds =
      let totalSeconds = seconds `mod` 86400
          hours :: Int = totalSeconds `div` 3600
          minutes :: Int = (totalSeconds `mod` 3600) `div` 60
          secs = fromIntegral $ totalSeconds `mod` 60
       in Time.TimeOfDay hours minutes secs

    diffTimeOfDay :: TimeOfDay -> TimeOfDay -> Int
    diffTimeOfDay t1 t2 = round $ toRational (Time.timeOfDayToTime t2 - Time.timeOfDayToTime t1)

getFrfsStations ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Context.City ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe DIBC.PlatformType ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Text ->
  Spec.VehicleCategory ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
getFrfsStations (_personId, mId) mbCity mbEndStationCode mbOrigin minimalData _platformType mbRouteCode mbStartStationCode vehicleType_ = do
  merchantOpCity <-
    case mbCity of
      Nothing ->
        CQMOC.findById (Id "407c445a-2200-c45f-8d67-6f6dbfa28e73")
          >>= fromMaybeM (MerchantOperatingCityNotFound "merchantOpCityId-407c445a-2200-c45f-8d67-6f6dbfa28e73")
      Just city ->
        CQMOC.findByMerchantIdAndCity mId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
  let platformType = fromMaybe (DIBC.APPLICATION) _platformType
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) platformType
  SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig ->
    case (mbRouteCode, mbStartStationCode, mbEndStationCode) of
      -- Return possible Start stops, when End Stop is Known
      (Nothing, Nothing, Just endStationCode) -> do
        currentTime <- getCurrentTime
        routesWithStop <- OTPRest.getRouteStopMappingByStopCode endStationCode integratedBPPConfig
        let routeCodes = nub $ map (.routeCode) routesWithStop
        routeStops <-
          EulerHS.Prelude.concatMapM
            (\routeCode -> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig)
            routeCodes
        let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
            groupedStopsByRouteCode = groupBy (\a b -> a.routeCode == b.routeCode) $ sortBy (compare `on` (.routeCode)) serviceableStops
            possibleStartStops =
              nubBy (\a b -> a.stopCode == b.stopCode) $
                concatMap
                  ( \stops ->
                      let mbEndStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == endStationCode) stops
                       in sortBy (compare `on` (.sequenceNum)) $ filter (\stop -> maybe False (\endStopSequence -> stop.stopCode /= endStationCode && stop.sequenceNum < endStopSequence) mbEndStopSequence) stops
                  )
                  groupedStopsByRouteCode
        let startStops =
              map
                ( \routeStop ->
                    FRFSStationAPI
                      { name = Just routeStop.stopName,
                        code = routeStop.stopCode,
                        routeCodes = Nothing,
                        lat = Just routeStop.stopPoint.lat,
                        lon = Just routeStop.stopPoint.lon,
                        integratedBppConfigId = integratedBPPConfig.id,
                        stationType = Nothing,
                        sequenceNum = Nothing,
                        address = Nothing,
                        distance = Nothing,
                        color = Nothing,
                        towards = Nothing,
                        timeTakenToTravelUpcomingStop = Nothing,
                        parentStopCode = Nothing
                      }
                )
                possibleStartStops
        mkStationsAPIWithDistance merchantOpCity integratedBPPConfig startStops mbOrigin
      -- Return possible End stops, when Route & Start Stop is Known
      (Just routeCode, Just startStationCode, Nothing) -> do
        routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
        currentTime <- getCurrentTime
        let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
            startSeqNum = fromMaybe 0 ((.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) serviceableStops)
            filteredRouteStops = filter (\stop -> stop.stopCode /= startStationCode && stop.sequenceNum > startSeqNum) serviceableStops
        let endStops =
              map
                ( \routeStop ->
                    FRFSStationAPI
                      { name = Just routeStop.stopName,
                        code = routeStop.stopCode,
                        routeCodes = Nothing,
                        lat = Just routeStop.stopPoint.lat,
                        lon = Just routeStop.stopPoint.lon,
                        stationType = Just (if routeStop.sequenceNum == 1 then START else if routeStop.sequenceNum < length filteredRouteStops then INTERMEDIATE else END),
                        sequenceNum = Just routeStop.sequenceNum,
                        integratedBppConfigId = integratedBPPConfig.id,
                        address = Nothing,
                        distance = Nothing,
                        color = Nothing,
                        towards = Nothing,
                        timeTakenToTravelUpcomingStop = Nothing,
                        parentStopCode = Nothing
                      }
                )
                filteredRouteStops
        mkStationsAPIWithDistance merchantOpCity integratedBPPConfig endStops mbOrigin
      -- Return all Stops, when only the Route is Known
      (Just routeCode, Nothing, Nothing) -> do
        currentTime <- getCurrentTime
        routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
        let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
            stopsSortedBySequenceNumber = sortBy (compare `on` RouteStopMapping.sequenceNum) serviceableStops
        let stops =
              map
                ( \routeStop ->
                    FRFSStationAPI
                      { name = Just routeStop.stopName,
                        code = routeStop.stopCode,
                        routeCodes = Nothing,
                        lat = Just routeStop.stopPoint.lat,
                        lon = Just routeStop.stopPoint.lon,
                        stationType = Just (if routeStop.sequenceNum == 1 then START else if routeStop.sequenceNum < length stopsSortedBySequenceNumber then INTERMEDIATE else END),
                        sequenceNum = Just routeStop.sequenceNum,
                        integratedBppConfigId = integratedBPPConfig.id,
                        address = Nothing,
                        distance = Nothing,
                        color = Nothing,
                        towards = Nothing,
                        timeTakenToTravelUpcomingStop = Nothing,
                        parentStopCode = Nothing
                      }
                )
                stopsSortedBySequenceNumber
        mkStationsAPIWithDistance merchantOpCity integratedBPPConfig stops mbOrigin
      -- Return all possible End Stops across all the Routes, when only the Start Stop is Known
      (Nothing, Just startStationCode, Nothing) -> do
        currentTime <- getCurrentTime
        routesWithStop <- OTPRest.getRouteStopMappingByStopCode startStationCode integratedBPPConfig
        let routeCodes = nub $ map (.routeCode) routesWithStop
        routeStops <- EulerHS.Prelude.concatMapM (\routeCode -> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig) routeCodes
        let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
            groupedStopsByRouteCode = groupBy (\a b -> a.routeCode == b.routeCode) $ sortBy (compare `on` (.routeCode)) serviceableStops
            possibleEndStops =
              groupBy (\a b -> a.stopCode == b.stopCode) $
                sortBy (compare `on` (.stopCode)) $
                  concatMap
                    ( \stops ->
                        let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                         in sortBy (compare `on` (.sequenceNum)) $ filter (\stop -> maybe False (\startStopSequence -> stop.stopCode /= startStationCode && stop.sequenceNum > startStopSequence) mbStartStopSequence) stops
                    )
                    groupedStopsByRouteCode
        let endStops =
              concatMap
                ( \routeStops' ->
                    case routeStops' of
                      routeStop : xs ->
                        let routeCodes' = nub $ routeStop.routeCode : map (.routeCode) xs
                         in [ FRFSStationAPI
                                { name = if fromMaybe False minimalData then Nothing else Just routeStop.stopName,
                                  code = routeStop.stopCode,
                                  routeCodes = Just routeCodes',
                                  lat = if fromMaybe False minimalData then Nothing else Just routeStop.stopPoint.lat,
                                  lon = if fromMaybe False minimalData then Nothing else Just routeStop.stopPoint.lon,
                                  integratedBppConfigId = integratedBPPConfig.id,
                                  stationType = Nothing,
                                  sequenceNum = Nothing,
                                  address = Nothing,
                                  distance = Nothing,
                                  color = Nothing,
                                  towards = Nothing,
                                  timeTakenToTravelUpcomingStop = Nothing,
                                  parentStopCode = Nothing
                                }
                            ]
                      _ -> []
                )
                possibleEndStops
        mkStationsAPIWithDistance merchantOpCity integratedBPPConfig endStops mbOrigin
      -- Return all the Stops
      _ -> do
        stations <- OTPRest.findAllStationsByVehicleType Nothing Nothing vehicleType_ integratedBPPConfig
        let stops =
              map
                ( \Station {..} ->
                    FRFSStationAPI
                      { color = Nothing,
                        distance = Nothing,
                        sequenceNum = Nothing,
                        stationType = Nothing,
                        towards = Nothing,
                        timeTakenToTravelUpcomingStop = Nothing,
                        name = Just name,
                        routeCodes = Nothing,
                        integratedBppConfigId = integratedBPPConfig.id,
                        ..
                      }
                )
                stations
        mkStationsAPIWithDistance merchantOpCity integratedBPPConfig stops mbOrigin
  where
    mkStationsAPIWithDistance merchantOpCity integratedBPPConfig stations = \case
      Just origin -> tryStationsAPIWithOSRMDistances mId merchantOpCity origin stations integratedBPPConfig
      Nothing -> return stations

postFrfsSearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) -> Spec.VehicleCategory -> API.Types.UI.FRFSTicketService.FRFSSearchAPIReq -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearch (mbPersonId, merchantId) mbCity mbIntegratedBPPConfigId vehicleType_ req = do
  let frfsRouteDetails =
        [ FRFSRouteDetails
            { routeCode = req.routeCode,
              startStationCode = req.fromStationCode,
              endStationCode = req.toStationCode,
              serviceTier = Nothing -- TODO: pass this for optimization
            }
        ]
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  merchantOperatingCityId <-
    case mbCity of
      Just city ->
        CQMOC.findByMerchantIdAndCity merchantId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show city)
          >>= return . (.id)
      Nothing ->
        CQP.findCityInfoById personId
          >>= fromMaybeM (PersonCityInformationNotFound personId.getId)
          >>= return . (.merchantOperatingCityId)

  merchantOperatingCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOperatingCityId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) DIBC.APPLICATION
  postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ req frfsRouteDetails Nothing Nothing Nothing Nothing (\_ -> pure ()) -- the journey leg upsert function is not required here

postFrfsDiscoverySearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postFrfsDiscoverySearch (_, merchantId) mbIntegratedBPPConfigId req = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId req.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> " ,city: " <> show req.city)
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOpCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory req.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory req.vehicleType) DIBC.APPLICATION
  CallExternalBPP.discoverySearch merchant bapConfig integratedBPPConfig req
  return Kernel.Types.APISuccess.Success

postFrfsSearchHandler ::
  (CallExternalBPP.FRFSSearchFlow m r, HasShortDurationRetryCfg r c) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  DMOC.MerchantOperatingCity ->
  DIBC.IntegratedBPPConfig ->
  Spec.VehicleCategory ->
  API.Types.UI.FRFSTicketService.FRFSSearchAPIReq ->
  [FRFSRouteDetails] ->
  Maybe (Id DPO.PartnerOrgTransaction) ->
  Maybe (Id DPO.PartnerOrganization) ->
  Maybe HighPrecMoney ->
  Maybe Text ->
  (Text -> m ()) ->
  m API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ FRFSSearchAPIReq {..} frfsRouteDetails mbPOrgTxnId mbPOrgId mbFare multimodalSearchRequestId upsertJourneyLegAction = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) >>= fromMaybeM (InternalError $ "Beckn Config not found " <> show merchantOperatingCity.id <> " " <> show merchant.id <> " " <> show vehicleType_)
  (fromStation, toStation) <- do
    fromStationInfo <- OTPRest.getStationByGtfsIdAndStopCode fromStationCode integratedBPPConfig >>= fromMaybeM (InvalidRequest $ "Invalid from station id: " <> fromStationCode <> " or integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
    toStationInfo <- OTPRest.getStationByGtfsIdAndStopCode toStationCode integratedBPPConfig >>= fromMaybeM (InvalidRequest $ "Invalid to station id: " <> toStationCode <> " or integratedBPPConfigId: " <> integratedBPPConfig.id.getId)
    return (fromStationInfo, toStationInfo)
  route <-
    maybe
      (pure Nothing)
      (\routeCode' -> OTPRest.getRouteByRouteId integratedBPPConfig routeCode')
      routeCode

  searchReqId <- generateGUID
  now <- getCurrentTime
  let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.searchTTLSec) now
      searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = vehicleType_,
            merchantId = merchantId,
            merchantOperatingCityId = fromStation.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            fromStationCode = fromStation.code,
            toStationCode = toStation.code,
            routeCode = route <&> (.code),
            riderId = personId,
            partnerOrgTransactionId = mbPOrgTxnId,
            partnerOrgId = mbPOrgId,
            integratedBppConfigId = integratedBPPConfig.id,
            isOnSearchReceived = Nothing,
            onSearchFailed = Nothing,
            validTill = Just validTill,
            searchAsParentStops = searchAsParentStops,
            ..
          }
  upsertJourneyLegAction searchReqId.getId
  QFRFSSearch.create searchReq
  CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq mbFare frfsRouteDetails integratedBPPConfig
  return $ FRFSSearchAPIRes searchReqId

getFrfsSearchQuote :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote (mbPersonId, _) searchId_ = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  search <- QFRFSSearch.findById searchId_ >>= fromMaybeM (InvalidRequest "Invalid search id")
  unless (personId == search.riderId) $ throwError AccessDenied
  (quotes :: [DFRFSQuote.FRFSQuote]) <- B.runInReplica $ QFRFSQuote.findAllBySearchId searchId_
  mapM
    ( \quote -> do
        (stations :: [FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
        let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
            fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
        singleAdultTicketPrice <- (find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.unitPrice)) & fromMaybeM (InternalError "Adult Ticket Unit Price not found.")
        adultQuantity <- (find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity)) & fromMaybeM (InternalError "Adult Ticket Quantity not found.")
        return $
          FRFSTicketService.FRFSQuoteAPIRes
            { quoteId = quote.id,
              _type = quote._type,
              price = singleAdultTicketPrice.amount,
              priceWithCurrency = mkPriceAPIEntity singleAdultTicketPrice,
              quantity = adultQuantity,
              validTill = quote.validTill,
              vehicleType = quote.vehicleType,
              discountedTickets = quote.discountedTickets,
              eventDiscountAmount = quote.eventDiscountAmount,
              integratedBppConfigId = quote.integratedBppConfigId,
              ..
            }
    )
    quotes

mkPaymentSuccessLockKey :: Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Text
mkPaymentSuccessLockKey bookingId = "frfsPaymentSuccess:" <> bookingId.getId

postFrfsQuoteV2Confirm :: (CallExternalBPP.FRFSConfirmFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId req = do
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quoteId
  let selectedQuoteCategories =
        map
          ( \quoteCategory ->
              let quantity = fromMaybe quoteCategory.selectedQuantity req.ticketQuantity
               in FRFSTicketService.FRFSCategorySelectionReq {quoteCategoryId = quoteCategory.id, quantity}
          )
          quoteCategories
  quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quote selectedQuoteCategories Nothing Nothing Nothing integratedBppConfig

postFrfsQuoteV2ConfirmUtil :: (CallExternalBPP.FRFSConfirmFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> DFRFSQuote.FRFSQuote -> [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq] -> Maybe MultimodalConfirm.CrisSdkResponse -> Maybe Bool -> Maybe Bool -> DIBC.IntegratedBPPConfig -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quote selectedQuoteCategories crisSdkResponse isSingleMode mbEnableOffer integratedBppConfig = do
  when (null selectedQuoteCategories) $ throwError $ NoSelectedCategoryFound quote.id.getId
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  mbBooking <- QFRFSTicketBooking.findBySearchId quote.searchId
  isMultiInitAllowed <-
    case mbBooking of
      Just booking -> do
        case integratedBppConfig.providerConfig of
          DIBC.ONDC DIBC.ONDCBecknConfig {multiInitAllowed} ->
            return $
              multiInitAllowed == Just True
                && booking.status `elem` [DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
          _ -> return $ booking.status `elem` [DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
      Nothing -> return True
  updatedQuoteCategories <-
    if isMultiInitAllowed
      then FRFSUtils.updateQuoteCategoriesWithQuantitySelections (selectedQuoteCategories <&> (\category -> (category.quoteCategoryId, category.quantity))) quoteCategories
      else return quoteCategories
  let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories updatedQuoteCategories)
  (rider, dConfirmRes) <- confirm isMultiInitAllowed fareParameters mbBooking
  merchantOperatingCity <- getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< dConfirmRes.routeStationsJson
  now <- getCurrentTime
  when isMultiInitAllowed $ do
    bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    when (dConfirmRes.status /= DFRFSTicketBooking.NEW) $ do
      void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.NEW dConfirmRes.id
    CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes' updatedQuoteCategories mbEnableOffer
  return $ makeBookingStatusAPI (dConfirmRes, updatedQuoteCategories) fareParameters routeStations stations merchantOperatingCity.city
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc
    confirm :: CallExternalBPP.FRFSConfirmFlow m r => Bool -> FRFSUtils.FRFSFareParameters -> Maybe DFRFSTicketBooking.FRFSTicketBooking -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    confirm isMultiInitAllowed fareParameters mbBooking = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ FRFSQuoteExpired quote.id.getId
      unless (personId == quote.riderId) $ throwError AccessDenied
      maybeM
        (buildAndCreateBooking rider quote fareParameters)
        ( \booking -> do
            updatedBooking <-
              if isMultiInitAllowed
                then do
                  let mBookAuthCode = crisSdkResponse <&> (.bookAuthCode)
                      totalPrice = fareParameters.totalPrice
                  void $ QFRFSTicketBooking.updateBookingAuthCodeById mBookAuthCode booking.id
                  void $ QFRFSTicketBooking.updateQuoteAndBppItemIdAndRouteStationsJson quote.id quote.bppItemId quote.routeStationsJson booking.id
                  -- TODO :: Update the status of the old payment booking to REATTEMPTED, Uncomment post release.
                  -- void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REATTEMPTED booking.id
                  void $ QFRFSTicketBooking.updateIsFareChangedById Nothing booking.id
                  return $ booking {DFRFSTicketBooking.quoteId = quote.id, DFRFSTicketBooking.bppItemId = quote.bppItemId, DFRFSTicketBooking.bookingAuthCode = mBookAuthCode, DFRFSTicketBooking.totalPrice = totalPrice}
                else return booking
            pure (rider, updatedBooking)
        )
        (pure mbBooking)

    buildAndCreateBooking :: CallExternalBPP.FRFSConfirmFlow m r => Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> FRFSUtils.FRFSFareParameters -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    buildAndCreateBooking rider quote'@DFRFSQuote.FRFSQuote {..} fareParameters = do
      uuid <- generateGUID
      now <- getCurrentTime
      mbSearch <- QFRFSSearch.findById searchId
      let isFareChanged = if isJust partnerOrgId then isJust oldCacheDump else False
      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote'.merchantId,
                totalPrice = fareParameters.totalPrice,
                paymentTxnId = Nothing,
                bppBankAccountNumber = Nothing,
                bppBankCode = Nothing,
                cancellationCharges = Nothing,
                refundAmount = Nothing,
                isBookingCancellable = Nothing,
                customerCancelled = False,
                payerVpa = Nothing,
                cashbackPayoutOrderId = Nothing,
                cashbackStatus = if isJust quote.discountedTickets then Just DFTB.PENDING else Nothing,
                bppDelayedInterest = quote.bppDelayedInterest,
                journeyOnInitDone = Nothing,
                startTime = Just now, -- TODO
                isFareChanged = Just isFareChanged,
                integratedBppConfigId = quote.integratedBppConfigId,
                googleWalletJWTUrl = Nothing,
                bookingAuthCode = crisSdkResponse <&> (.bookAuthCode),
                osType = crisSdkResponse <&> (.osType),
                osBuildVersion = crisSdkResponse <&> (.osBuildVersion),
                recentLocationId = mbSearch >>= (.recentLocationId),
                failureReason = Nothing,
                isSingleMode = isSingleMode,
                ..
              }
      QFRFSTicketBooking.create booking

      -- Update userBookedRouteShortName and userBookedBusServiceTierType from route_stations_json
      let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< routeStationsJson
      let mbFirstRouteStation = listToMaybe (fromMaybe [] routeStations)
      let mbBookedRouteShortName = mbFirstRouteStation <&> (.shortName)
      let mbBookedServiceTierType = mbFirstRouteStation >>= (.vehicleServiceTier) <&> (._type)
      when (isJust mbBookedRouteShortName && isJust mbBookedServiceTierType) $ do
        mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just searchId.getId)
        whenJust mbJourneyLeg $ \journeyLeg -> do
          whenJust mbBookedRouteShortName $ \bookedRouteShortName ->
            QRouteDetails.updateUserBookedRouteShortName (Just bookedRouteShortName) journeyLeg.id.getId
          QJourneyLeg.updateByPrimaryKey $ journeyLeg {DJL.userBookedBusServiceTierType = mbBookedServiceTierType}

      return (rider, booking)

    makeBookingStatusAPI (booking, quoteCategories) fareParameters routeStations stations city = do
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
          price = Just booking.totalPrice.amount,
          priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
          quantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity),
          validTill = booking.validTill,
          vehicleType = booking.vehicleType,
          status = booking.status,
          payment = Nothing,
          tickets = [],
          discountedTickets = booking.discountedTickets,
          eventDiscountAmount = booking.eventDiscountAmount,
          isFareChanged = booking.isFareChanged,
          googleWalletJWTUrl = booking.googleWalletJWTUrl,
          integratedBppConfigId = booking.integratedBppConfigId,
          ..
        }

postFrfsQuoteConfirm :: (CallExternalBPP.FRFSConfirmFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId = do
  postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId (API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq {offered = Nothing, ticketQuantity = Nothing, childTicketQuantity = Nothing})

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

frfsOrderStatusHandler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
  ) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  DPayment.PaymentStatusResp ->
  (DJL.JourneyLeg -> Id DFRFSQuote.FRFSQuote -> m ()) ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
frfsOrderStatusHandler merchantId paymentStatusResponse switchFRFSQuoteTier = do
  orderShortId <- DPayment.getOrderShortId paymentStatusResponse
  logDebug $ "frfs ticket order bap webhookc call" <> orderShortId.getShortId
  order <- QPaymentOrder.findByShortId orderShortId >>= fromMaybeM (PaymentOrderNotFound orderShortId.getShortId)
  bookingPayments <- QFRFSTicketBookingPayment.findAllByOrderId order.id
  bookingsStatusWithBooking <-
    mapM
      ( \bookingPayment -> do
          booking <- QFRFSTicketBooking.findById bookingPayment.frfsTicketBookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
          person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
          paymentOrder <- QPaymentOrder.findById bookingPayment.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found")
          bookingStatus <- frfsBookingStatus (booking.riderId, merchantId) False (withPaymentStatusResponseHandler bookingPayment paymentOrder) booking person switchFRFSQuoteTier
          return (bookingStatus, booking)
      )
      bookingPayments
  let (bookingsStatus, bookings) = unzip bookingsStatusWithBooking
  let firstBooking = listToMaybe bookings
  journeyId <- maybe (pure Nothing) getJourneyIdFromBooking firstBooking
  return $
    ( evaluateConditions
        [ (Just DFRFSTicketBooking.CONFIRMED, Nothing, DPayment.FulfillmentSucceeded, all), -- Ticket Generated
          (Nothing, Just FRFSTicketService.REFUND_PENDING, DPayment.FulfillmentRefundPending, all), -- Paid But Refund Pending (Could be due to Booking Cancellation/Failure)
          (Nothing, Just FRFSTicketService.REFUND_INITIATED, DPayment.FulfillmentRefundInitiated, all), -- Paid But Refund Initiated (Could be due to Booking Cancellation/Failure)
          (Nothing, Just FRFSTicketService.REFUND_FAILED, DPayment.FulfillmentRefundFailed, all), -- Paid But Refund Failed (Could be due to Booking Cancellation/Failure)
          (Nothing, Just FRFSTicketService.REFUNDED, DPayment.FulfillmentRefunded, all), -- Paid But Refunded (Could be due to Booking Cancellation/Failure)
          (Just DFRFSTicketBooking.FAILED, Just FRFSTicketService.FAILURE, DPayment.FulfillmentFailed, all), -- Booking Payment Failed
          (Just DFRFSTicketBooking.FAILED, Just FRFSTicketService.SUCCESS, DPayment.FulfillmentFailed, any) -- Paid but Booking Failed
        ]
        bookingsStatus,
      journeyId <&> (.getId),
      Nothing
    )
  where
    -- evaluateConditions :: Foldable t => [(Maybe DFRFSTicketBooking.FRFSTicketBookingStatus, Maybe FRFSTicketService.FRFSBookingPaymentStatusAPI, Payment.PaymentFulfillmentStatus, ((a -> Bool) -> t a -> Bool))] -> [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes] -> Payment.PaymentFulfillmentStatus
    evaluateConditions [] _ = DPayment.FulfillmentPending
    evaluateConditions (condition : conditions) bookings = do
      let (mbBookingStatus, mbBookingPaymentStatus, paymentFulfillmentStatus, conditionFn) = condition
      if conditionFn
        ( \booking ->
            maybe True (\bookingStatus -> bookingStatus == booking.status) mbBookingStatus
              && maybe True (\paymentStatus -> Just paymentStatus == (booking.payment <&> (.status))) mbBookingPaymentStatus
        )
        bookings
        then paymentFulfillmentStatus
        else evaluateConditions conditions bookings

    withPaymentStatusResponseHandler ::
      ( EncFlow m r,
        EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        SchedulerFlow r,
        HasBAPMetrics m r,
        HasFlowEnv m r '["smsCfg" ::: SmsConfig],
        HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
      ) =>
      DFRFSTicketBookingPayment.FRFSTicketBookingPayment ->
      DPaymentOrder.PaymentOrder ->
      ( (DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, DPayment.PaymentStatusResp) ->
        m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      ) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler paymentBooking paymentOrder action = action (paymentBooking, paymentOrder, paymentStatusResponse)

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  frfsBookingStatus (personId, merchantId_) False (withPaymentStatusResponseHandler booking person) booking person (\_ _ -> pure ())
  where
    withPaymentStatusResponseHandler ::
      ( EncFlow m r,
        EsqDBFlow m r,
        CacheFlow m r,
        MonadFlow m,
        EsqDBReplicaFlow m r,
        ServiceFlow m r,
        SchedulerFlow r
      ) =>
      DFRFSTicketBooking.FRFSTicketBooking ->
      DP.Person ->
      ((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler booking person action = do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person booking.riderId
      let orderStatusCall = Payment.orderStatus booking.merchantId booking.merchantOperatingCityId Nothing (getPaymentType booking.vehicleType) (Just person.id.getId) person.clientSdkVersion
      paymentStatusResponse <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
      action (paymentBooking, paymentOrder, paymentStatusResponse)

    getPaymentType = \case
      Spec.METRO -> Payment.FRFSBooking
      Spec.SUBWAY -> Payment.FRFSBooking
      Spec.BUS -> Payment.FRFSBusBooking

-- pass isMultiModalBooking = True in case of multimodal flow
frfsBookingStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
  ) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Bool ->
  (((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  DP.Person ->
  (DJL.JourneyLeg -> Id DFRFSQuote.FRFSQuote -> m ()) ->
  m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
frfsBookingStatus (personId, merchantId_) isMultiModalBooking withPaymentStatusResponseHandler booking' person switchFRFSQuoteTier = do
  logInfo $ "frfsBookingStatus for booking: " <> show booking'
  let bookingId = booking'.id
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking'.merchantOperatingCityId merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking'.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  now <- getCurrentTime
  let validTillWithBuffer = addUTCTime 5 booking'.validTill
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.status /= DFRFSTicketBooking.CANCELLED && validTillWithBuffer < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  logInfo $ "Booking status: " <> show booking.status
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.FAILED -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "payment status resp: " <> show paymentStatusResp
        let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
        logInfo $ "payment booking status: " <> show paymentBookingStatus
        when (paymentBookingStatus == FRFSTicketService.FAILURE) do
          void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
          let mPrice = Common.mkPrice (Just booking'.totalPrice.currency) (HighPrecMoney $ toRational (0 :: Int))
          void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById mPrice mPrice booking.id
        when (paymentBookingStatus == FRFSTicketService.SUCCESS) do
          void $ markJourneyPaymentSuccess booking paymentOrder paymentBooking
        when (paymentBookingStatus == FRFSTicketService.PENDING) do
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
          void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.PENDING paymentBooking.id
        let paymentStatusAPI =
              case paymentBooking.status of
                DFRFSTicketBookingPayment.REFUND_PENDING -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_PENDING
                DFRFSTicketBookingPayment.REFUND_INITIATED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_INITIATED
                DFRFSTicketBookingPayment.REFUND_FAILED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_FAILED
                DFRFSTicketBookingPayment.REFUNDED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                _ ->
                  case paymentBookingStatus of
                    FRFSTicketService.REFUNDED -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                    FRFSTicketService.FAILURE -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
                    FRFSTicketService.SUCCESS -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
                    FRFSTicketService.PENDING -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.PENDING
                    _ -> Nothing
        logInfo $ "payment status api: " <> show paymentStatusAPI
        let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
        buildFRFSTicketBookingStatusAPIRes booking quoteCategories mbPaymentObj
    DFRFSTicketBooking.CONFIRMING -> do
      if addUTCTime 5 booking.validTill < now
        then do
          logInfo $ "booking is expired in confirming: " <> show booking
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
        else do
          buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      fork "FRFS Booking Status" $ CallExternalBPP.status merchant.id merchantOperatingCity bapConfig booking
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "payment status response: " <> show paymentStatusResp
        let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
        if paymentBookingStatus == FRFSTicketService.FAILURE
          then do
            logInfo $ "payment failed in approved: " <> show booking
            QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
            QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
            let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
            buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
          else
            if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
              then do
                logInfo $ "booking is expired in approved: " <> show booking
                void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
              else do
                txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
                let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing Nothing
                paymentOrder_ <- buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCity.id booking
                let paymentObj =
                      Just $
                        FRFSTicketService.FRFSBookingPaymentAPI
                          { status = paymentStatus_,
                            paymentOrder = paymentOrder_,
                            transactionId = Nothing
                          }
                buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      withPaymentStatusResponseHandler $ \(paymentBooking, paymentOrder, paymentStatusResp) -> do
        logInfo $ "paymentStatusResp: " <> show paymentStatusResp
        let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
        logInfo $ "paymentBookingStatus: " <> show paymentBookingStatus
        bookingApiResp <-
          if paymentBookingStatus == FRFSTicketService.FAILURE
            then do
              logInfo $ "payment failed in payment pending: " <> show booking <> ", status: " <> show paymentBookingStatus
              QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
              QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.FAILED paymentBooking.id
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
            else
              if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
                then do
                  logInfo $ "booking is expired in payment success and booking is expired: " <> show booking
                  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                  void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                  buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
                else
                  if paymentBookingStatus == FRFSTicketService.SUCCESS
                    then do
                      -- Add default TTL of 1 min or the value provided in the config
                      let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
                      transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
                      txnId <- getSuccessTransactionId transactions
                      isLockAcquired <- Hedis.tryLockRedis (mkPaymentSuccessLockKey bookingId) 60
                      if isLockAcquired
                        then do
                          latestBookingPayment <- QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
                          let isPaymentBookingLatest = maybe True (\lpb -> lpb.id == paymentBooking.id) latestBookingPayment
                          if isPaymentBookingLatest
                            then do
                              void $ QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.SUCCESS paymentBooking.id
                              void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id
                              mbJourneyLeg <- markJourneyPaymentSuccess booking paymentOrder paymentBooking
                              quoteUpdatedBooking <- maybeM (pure booking) pure (QFRFSTicketBooking.findById bookingId)
                              let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                              mRiderNumber <- mapM decrypt person.mobileNumber
                              void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent paymentStatusResp.payerVpa bookingId
                              mbJourney <- case mbJourneyLeg of
                                Just journeyLeg -> do
                                  QJourney.findByPrimaryKey journeyLeg.journeyId
                                Nothing -> pure Nothing
                              let mbIsSingleMode = mbJourney >>= (.isSingleMode)
                              void $ CallExternalBPP.confirm processOnConfirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) quoteUpdatedBooking quoteCategories mbIsSingleMode
                              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL) (Just txnId.getId)
                              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentSuccess
                            else do
                              logError $ "booking payment for which order was charged is older: " <> show booking
                              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
                              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                              void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                              buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories paymentFailed
                        else buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentSuccess
                    else do
                      if paymentBookingStatus == FRFSTicketService.REFUNDED
                        then do
                          logInfo $ "payment failed in payment pending: " <> show booking <> ", status: " <> show paymentBookingStatus
                          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
                          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
                          QFRFSTicketBookingPayment.updateStatusById DFRFSTicketBookingPayment.REFUNDED paymentBooking.id
                          void $ markJourneyPaymentSuccess updatedBooking paymentOrder paymentBooking
                          let paymentStatusAPI = Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUNDED
                          let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
                          buildFRFSTicketBookingStatusAPIRes updatedBooking quoteCategories mbPaymentObj
                        else do
                          logInfo $ "payment success in payment pending: " <> show booking
                          paymentOrder_ <- buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCity.id booking
                          txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                          let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                              paymentObj =
                                Just
                                  FRFSTicketService.FRFSBookingPaymentAPI
                                    { status = paymentStatus_,
                                      paymentOrder = paymentOrder_,
                                      transactionId = Nothing
                                    }
                          buildFRFSTicketBookingStatusAPIRes booking quoteCategories paymentObj
        when (isMultiModalBooking && paymentBookingStatus == FRFSTicketService.SUCCESS) $ do
          riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
          becknConfigs <- CQBC.findByMerchantIdDomainandMerchantOperatingCityId merchantId_ "FRFS" merchantOperatingCity.id
          let initTTLs = map (.initTTLSec) becknConfigs
          let maxInitTTL = intToNominalDiffTime $ case catMaybes initTTLs of
                [] -> 0 -- 30 minutes in seconds if all are Nothing
                ttlList -> maximum ttlList
          let bufferTime = case riderConfig.refundBufferTTLSec of
                Just secs -> secs.getSeconds
                Nothing -> 2 * 60
          let scheduleAfter = maxInitTTL + (intToNominalDiffTime bufferTime) -- schedule job (maxInitTTL + bufferTime) after calling confirm
              jobData = JobScheduler.CheckMultimodalConfirmFailJobData {JobScheduler.bookingId = bookingId}
          createJobIn @_ @'CheckMultimodalConfirmFail (Just merchantId_) (Just merchantOperatingCity.id) scheduleAfter (jobData :: CheckMultimodalConfirmFailJobData)
        return bookingApiResp
    DFRFSTicketBooking.CANCELLED -> do
      FRFSUtils.updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      let mbPaymentObj = paymentBooking <&> \tbp -> FRFSTicketService.FRFSBookingPaymentAPI {status = Utils.mkTBPStatusAPI tbp.status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories mbPaymentObj
    DFRFSTicketBooking.COUNTER_CANCELLED -> do
      FRFSUtils.updateTotalOrderValueAndSettlementAmount booking quoteCategories bapConfig
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.CANCEL_INITIATED -> do
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    DFRFSTicketBooking.TECHNICAL_CANCEL_REJECTED -> do
      buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
  where
    markJourneyPaymentSuccess booking paymentOrder paymentBooking = do
      mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just booking.searchId.getId)
      whenJust mbJourneyLeg $ \journeyLeg -> do
        let journeyId = journeyLeg.journeyId
        whenJust paymentBooking.frfsQuoteId $ \paymentBookingQuoteId -> do
          when (booking.quoteId /= paymentBookingQuoteId) $ do
            switchFRFSQuoteTier journeyLeg paymentBookingQuoteId
        void $ QJourney.updatePaymentOrderShortId (Just paymentOrder.shortId) (Just True) journeyId
        void $ QJourney.updateStatus (if booking.status == DFRFSTicketBooking.FAILED then DJ.FAILED else DJ.INPROGRESS) journeyId
      pure mbJourneyLeg

    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    processOnConfirm ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        MonadFlow m,
        EncFlow m r,
        SchedulerFlow r,
        HasBAPMetrics m r,
        HasFlowEnv m r '["smsCfg" ::: SmsConfig],
        HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
        HasKafkaProducer r,
        HasLongDurationRetryCfg r c,
        HasShortDurationRetryCfg r c,
        CallFRFSBPP.BecknAPICallFlow m r,
        HasFlowEnv m r '["googleSAPrivateKey" ::: String]
      ) =>
      FRFSCommon.DOrder ->
      m ()
    processOnConfirm onConfirmReq = do
      (merchant', booking'', quoteCategories') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking'' quoteCategories' onConfirmReq

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    buildCreateOrderResp paymentOrder commonPersonId merchantOperatingCityId booking = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
      isSplitEnabled_ <- Payment.getIsSplitEnabled merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
      isPercentageSplitEnabled <- Payment.getIsPercentageSplit merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
      let isSingleMode = fromMaybe False booking.isSingleMode
      splitSettlementDetails <- Payment.mkSplitSettlementDetails isSplitEnabled_ paymentOrder.amount [] isPercentageSplitEnabled isSingleMode
      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = paymentOrder.id.getId,
                orderShortId = paymentOrder.shortId.getShortId,
                amount = paymentOrder.amount,
                customerId = person.id.getId,
                customerEmail = fromMaybe "growth@nammayatri.in" personEmail,
                customerPhone = personPhone,
                customerFirstName = person.firstName,
                customerLastName = person.lastName,
                createMandate = Nothing,
                mandateMaxAmount = Nothing,
                mandateFrequency = Nothing,
                mandateEndDate = Nothing,
                mandateStartDate = Nothing,
                optionsGetUpiDeepLinks = Nothing,
                metadataExpiryInMins = Nothing,
                metadataGatewayReferenceId = Nothing, --- assigned in shared kernel
                splitSettlementDetails = splitSettlementDetails,
                basket = Nothing
              }
      mbPaymentOrderValidTill <- Payment.getPaymentOrderValidity merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
      DPayment.createOrderService commonMerchantId (Just $ cast merchantOperatingCityId) commonPersonId mbPaymentOrderValidTill Nothing (getPaymentType booking.vehicleType) createOrderReq (createOrderCall merchantOperatingCityId booking (Just person.id.getId) person.clientSdkVersion)

    getPaymentType = \case
      Spec.METRO -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBooking
      Spec.SUBWAY -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBooking
      Spec.BUS -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBusBooking

    createOrderCall merchantOperatingCityId booking mRoutingId sdkVersion = Payment.createOrder merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType) mRoutingId sdkVersion

    commonMerchantId = Kernel.Types.Id.cast @Merchant.Merchant @DPayment.Merchant merchantId_

    makeUpdatedBooking DFRFSTicketBooking.FRFSTicketBooking {..} updatedStatus mTTL transactionId =
      let validTill' = mTTL & fromMaybe validTill
          newPaymentTxnId = transactionId <|> paymentTxnId
       in DFRFSTicketBooking.FRFSTicketBooking {status = updatedStatus, validTill = validTill', paymentTxnId = newPaymentTxnId, ..}
    getSuccessTransactionId transactions = do
      let successTransactions = filter (\transaction -> transaction.status == Payment.CHARGED) transactions
      case successTransactions of
        [] -> throwError $ InvalidRequest "No successful transaction found"
        [transaction] -> return transaction.id
        _ -> throwError $ InvalidRequest "Multiple successful transactions found"

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Maybe Spec.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, _merchantId) mbLimit mbOffset mbVehicleCategory = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId mbLimit mbOffset personId mbVehicleCategory
  mapM
    ( \booking -> do
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId booking.quoteId
        buildFRFSTicketBookingStatusAPIRes booking quoteCategories Nothing
    )
    bookings

buildFRFSTicketBookingStatusAPIRes ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c
  ) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  [FRFSQuoteCategory.FRFSQuoteCategory] ->
  Maybe FRFSTicketService.FRFSBookingPaymentAPI ->
  m FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking quoteCategories payment = do
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  stations <- mapM (Utils.mkPOrgStationAPI booking.partnerOrgId integratedBppConfig) =<< (decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db"))
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {..} ->
              FRFSTicketService.FRFSTicketAPI {..}
          )
          tickets'
      fareParameters = FRFSUtils.mkFareParameters (mkCategoryPriceItemFromQuoteCategories quoteCategories)
  return $
    FRFSTicketService.FRFSTicketBookingStatusAPIRes
      { bookingId = booking.id,
        city = merchantOperatingCity.city,
        updatedAt = booking.updatedAt,
        createdAt = booking.createdAt,
        _type = booking._type,
        quoteCategories = map mkFRFSQuoteCategoryAPIEntity quoteCategories,
        price = Just $ booking.totalPrice.amount,
        priceWithCurrency = Just $ mkPriceAPIEntity booking.totalPrice,
        quantity = find (\category -> category.categoryType == ADULT) fareParameters.priceItems <&> (.quantity),
        validTill = booking.validTill,
        vehicleType = booking.vehicleType,
        status = booking.status,
        discountedTickets = booking.discountedTickets,
        eventDiscountAmount = booking.eventDiscountAmount,
        payment = payment <&> (\p -> p {transactionId = booking.paymentTxnId}),
        isFareChanged = booking.isFareChanged,
        googleWalletJWTUrl = booking.googleWalletJWTUrl,
        integratedBppConfigId = booking.integratedBppConfigId,
        ..
      }

mkFRFSQuoteCategoryAPIEntity :: FRFSQuoteCategory.FRFSQuoteCategory -> FRFSTicketService.FRFSQuoteCategoryAPIEntity
mkFRFSQuoteCategoryAPIEntity FRFSQuoteCategory.FRFSQuoteCategory {..} =
  FRFSTicketService.FRFSQuoteCategoryAPIEntity {categoryMetadata = mkCategoryMetadataAPIEntity <$> categoryMeta, price = mkPriceAPIEntity price, offeredPrice = mkPriceAPIEntity offeredPrice, finalPrice = mkPriceAPIEntity <$> finalPrice, ..}
  where
    mkCategoryMetadataAPIEntity FRFSQuoteCategory.QuoteCategoryMetadata {..} = FRFSTicketService.FRFSTicketCategoryMetadataAPIEntity {..}

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus Payment.NEW = FRFSTicketService.NEW
makeTicketBookingPaymentAPIStatus PENDING_VBV = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus CHARGED = FRFSTicketService.SUCCESS
makeTicketBookingPaymentAPIStatus AUTHENTICATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus JUSPAY_DECLINED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZING = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus COD_INITIATED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus STARTED = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus AUTO_REFUNDED = FRFSTicketService.REFUNDED
makeTicketBookingPaymentAPIStatus CLIENT_AUTH_TOKEN_EXPIRED = FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus Payment.CANCELLED = FRFSTicketService.FAILURE

cancelFRFSTicketBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow ()
cancelFRFSTicketBooking booking = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id

postFrfsBookingCanCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCanCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show ticketBooking.merchantOperatingCityId)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.SOFT_CANCEL ticketBooking
  return APISuccess.Success

getFrfsBookingCanCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
getFrfsBookingCanCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid ticketBookingId")
  return $
    FRFSCanCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = getAbsoluteValue ticketBooking.refundAmount,
        isCancellable = ticketBooking.isBookingCancellable
      }

postFrfsBookingCancel :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow APISuccess.APISuccess
postFrfsBookingCancel (_, merchantId) bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  merchantOperatingCity <- CQMOC.findById ticketBooking.merchantOperatingCityId >>= fromMaybeM (InvalidRequest $ "Invalid merchant operating city id" <> ticketBooking.merchantOperatingCityId.getId)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory ticketBooking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  void $ CallExternalBPP.cancel merchant merchantOperatingCity bapConfig Spec.CONFIRM_CANCEL ticketBooking
  return APISuccess.Success

getFrfsBookingCancelStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow FRFSTicketService.FRFSCancelStatus
getFrfsBookingCancelStatus _ bookingId = do
  ticketBooking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  pure
    FRFSTicketService.FRFSCancelStatus
      { cancellationCharges = getAbsoluteValue ticketBooking.cancellationCharges,
        refundAmount = if ticketBooking.status == DFRFSTicketBooking.CANCELLED then getAbsoluteValue ticketBooking.refundAmount else Nothing
      }

getAbsoluteValue :: Maybe HighPrecMoney -> Maybe HighPrecMoney
getAbsoluteValue mbRefundAmount = case mbRefundAmount of
  Nothing -> Nothing
  Just rfValue -> do
    let HighPrecMoney value = rfValue
    Just (HighPrecMoney $ abs value)

getFrfsConfig :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Context.City -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSConfigAPIRes
getFrfsConfig (pId, mId) opCity = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity mId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> " ,city: " <> show opCity)
  Domain.Types.FRFSConfig.FRFSConfig {..} <- B.runInReplica $ CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOpCity.id [] >>= fromMaybeM (InvalidRequest "FRFS Config not found")
  stats <- maybe (pure Nothing) CQP.findPersonStatsById pId
  let isEventOngoing' = fromMaybe False isEventOngoing
      ticketsBookedInEvent = fromMaybe 0 ((.ticketsBookedInEvent) =<< stats)
  return FRFSTicketService.FRFSConfigAPIRes {isEventOngoing = isEventOngoing', ..}

-- TODO :: Filter the Stops which are always the END stop for all the routes as it can never be a possible START or INTERMEDIATE stop.
getFrfsAutocomplete ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Text ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe Int ->
  Kernel.Prelude.Maybe DIBC.PlatformType ->
  Context.City ->
  Kernel.External.Maps.Types.LatLong ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  Environment.Flow API.Types.UI.FRFSTicketService.AutocompleteRes
getFrfsAutocomplete (_, mId) mbInput mbLimit mbOffset _platformType opCity origin vehicle = do
  merchantOpCity <- CQMOC.findByMerchantIdAndCity mId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> " ,city: " <> show opCity)
  let platformType = fromMaybe (DIBC.APPLICATION) _platformType
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow merchantOpCity.id []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show merchantOpCity.id)
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicle) platformType

  stops <-
    SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig ->
      case mbInput of
        Nothing -> do
          allStops <- OTPRest.findAllStationsByVehicleType Nothing Nothing vehicle integratedBPPConfig
          currentTime <- getCurrentTime
          let serviceableStops = DTB.findBoundedDomain allStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) allStops
              stopsWithinRadius =
                filter
                  ( \stop ->
                      let straightLineDist = highPrecMetersToMeters (CD.distanceBetweenInMeters origin (LatLong (fromMaybe merchantOpCity.lat stop.lat) (fromMaybe merchantOpCity.long stop.lon)))
                       in straightLineDist <= frfsConfig.straightLineDistance
                  )
                  serviceableStops

          stopsWithDistance <- tryStationsAPIWithOSRMDistances mId merchantOpCity origin (mkStationsAPI integratedBPPConfig stopsWithinRadius) integratedBPPConfig
          let stopsWithinActualRadius = filter (\stop -> maybe True (\distance -> distance <= frfsConfig.radius) stop.distance) stopsWithDistance
          return stopsWithinActualRadius
        Just userInput -> do
          matchingStops <- OTPRest.findAllMatchingStations (Just userInput) mbLimit mbOffset vehicle integratedBPPConfig
          currentTime <- getCurrentTime
          let serviceableStops = DTB.findBoundedDomain matchingStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) matchingStops
          stopsWithDistance <- tryStationsAPIWithOSRMDistances mId merchantOpCity origin (mkStationsAPI integratedBPPConfig (filter (\stop -> maybe True (any (`elem` [END, INTERMEDIATE])) stop.possibleTypes) serviceableStops)) integratedBPPConfig
          return stopsWithDistance
  routes <-
    SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig ->
      case mbInput of
        Nothing -> return []
        Just userInput -> do
          matchingRoutes <- OTPRest.findAllMatchingRoutes (Just userInput) mbLimit mbOffset vehicle integratedBPPConfig
          currentTime <- getCurrentTime
          let serviceableRoutes = DTB.findBoundedDomain matchingRoutes currentTime ++ filter (\route -> route.timeBounds == DTB.Unbounded) matchingRoutes

          let routes =
                map
                  ( \route ->
                      FRFSRouteAPI
                        { code = route.code,
                          shortName = route.shortName,
                          longName = route.longName,
                          startPoint = route.startPoint,
                          endPoint = route.endPoint,
                          timeBounds = Just route.timeBounds,
                          totalStops = Nothing,
                          stops = Nothing,
                          waypoints = Nothing,
                          integratedBppConfigId = integratedBPPConfig.id
                        }
                  )
                  serviceableRoutes
          return routes
  return API.Types.UI.FRFSTicketService.AutocompleteRes {routes = routes, stops = stops}
  where
    mkStationsAPI integratedBPPConfig =
      map
        ( \Station {..} ->
            FRFSStationAPI
              { color = Nothing,
                routeCodes = Nothing,
                distance = Nothing,
                sequenceNum = Nothing,
                stationType = Nothing,
                towards = Nothing,
                timeTakenToTravelUpcomingStop = Nothing,
                name = Just name,
                integratedBppConfigId = integratedBPPConfig.id,
                ..
              }
        )

postFrfsTicketVerify ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe DIBC.PlatformType ->
  Context.City ->
  BecknV2.FRFS.Enums.VehicleCategory ->
  API.Types.UI.FRFSTicketService.FRFSTicketVerifyReq ->
  Environment.Flow APISuccess.APISuccess
postFrfsTicketVerify (_mbPersonId, merchantId) _platformType opCity vehicleCategory req = do
  merchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> "-city-" <> show opCity)
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchantId (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleCategory) >>= fromMaybeM (InternalError "Beckn Config not found")
  let platformType = fromMaybe (DIBC.APPLICATION) _platformType
  _ <- CallExternalBPP.verifyTicket merchantId merchantOperatingCity bapConfig vehicleCategory req.qrData platformType
  return APISuccess.Success

postFrfsBookingFeedback ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking ->
    API.Types.UI.FRFSTicketService.FRFSBookingFeedbackReq ->
    Environment.Flow APISuccess.APISuccess
  )
postFrfsBookingFeedback (_mbPersonId, merchantId) bookingId req = do
  -- Validate merchant exists
  void $ CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")

  let (isFareAccepted, feedbackDetails) = case req of
        API.Types.UI.FRFSTicketService.BookingFareAccepted fareAcceptedReq -> (Just fareAcceptedReq.isFareAccepted, Nothing)
        API.Types.UI.FRFSTicketService.BookingFeedback feedbackReq -> (Nothing, Just feedbackReq.feedbackDetails)

  existingFeedback <- QFRFSTicketBookingFeedback.findByBookingId bookingId
  case existingFeedback of
    Just _ -> void $ QFRFSTicketBookingFeedback.updateByBookingId isFareAccepted feedbackDetails bookingId
    Nothing -> do
      feedbackId <- generateGUID
      now <- getCurrentTime
      let feedback =
            DFRFSTicketBookingFeedback.FRFSTicketBookingFeedback
              { id = feedbackId,
                bookingId = bookingId,
                isFareAccepted = isFareAccepted,
                feedbackDetails = feedbackDetails,
                merchantId = merchantId,
                merchantOperatingCityId = booking.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      void $ QFRFSTicketBookingFeedback.create feedback

  return APISuccess.Success

tryStationsAPIWithOSRMDistances :: Id Merchant.Merchant -> MerchantOperatingCity -> LatLong -> [FRFSStationAPI] -> DIBC.IntegratedBPPConfig -> Environment.Flow [FRFSStationAPI]
tryStationsAPIWithOSRMDistances merchantId merchantOpCity origin stops integratedBPPConfig = do
  if null stops
    then return []
    else do
      let transformedStops =
            map
              ( \stop ->
                  StationResult
                    { lat = fromMaybe merchantOpCity.lat stop.lat,
                      lon = fromMaybe merchantOpCity.long stop.lon,
                      code = stop.code,
                      routeCodes = stop.routeCodes,
                      name = stop.name,
                      stationType = stop.stationType,
                      sequenceNum = stop.sequenceNum,
                      parentStopCode = stop.parentStopCode
                    }
              )
              stops

      let maxBatchSize = 100
          stopBatches = chunksOf maxBatchSize transformedStops

      batchedResults <- fmap concat $
        forM stopBatches $ \batch -> do
          res <-
            withTryCatch "getFrfsAutocompleteDistances:tryStationsAPIWithOSRMDistances" $
              Maps.getFrfsAutocompleteDistances merchantId merchantOpCity.id Nothing $
                GetDistancesReq
                  { origins = NonEmpty.fromList batch,
                    destinations = NonEmpty.fromList [origin],
                    distanceUnit = Meter,
                    sourceDestinationMapping = Nothing,
                    travelMode = Just Maps.CAR
                  }
          case res of
            Left _ -> return $ map (\StationResult {..} -> FRFSStationAPI {lat = Just lat, lon = Just lon, address = Nothing, color = Nothing, distance = Nothing, towards = Nothing, timeTakenToTravelUpcomingStop = Nothing, integratedBppConfigId = integratedBPPConfig.id, ..}) batch
            Right stopsDistanceResp ->
              return $ map (\stop -> mkStopToAPI stop.origin stop.distance) (NonEmpty.toList stopsDistanceResp)

      return batchedResults
  where
    mkStopToAPI stop distance =
      FRFSStationAPI
        { name = stop.name,
          code = stop.code,
          routeCodes = stop.routeCodes,
          lat = Just stop.lat,
          lon = Just stop.lon,
          distance = Just distance,
          stationType = stop.stationType,
          sequenceNum = stop.sequenceNum,
          parentStopCode = stop.parentStopCode,
          timeTakenToTravelUpcomingStop = Nothing,
          address = Nothing,
          color = Nothing,
          towards = Nothing,
          integratedBppConfigId = integratedBPPConfig.id
        }

getMerchantOperatingCityFromBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m DMOC.MerchantOperatingCity
getMerchantOperatingCityFromBooking tBooking = do
  let moCityId = tBooking.merchantOperatingCityId
  CQMOC.findById moCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show moCityId)

postFrfsStationsPossibleStops ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Maybe Context.City ->
  Kernel.Prelude.Maybe DIBC.PlatformType ->
  Spec.VehicleCategory ->
  API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq ->
  Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
postFrfsStationsPossibleStops (_personId, mId) mbCity _platformType vehicleType_ req = do
  merchantOpCity <-
    case mbCity of
      Nothing ->
        CQMOC.findById (Id "407c445a-2200-c45f-8d67-6f6dbfa28e73")
          >>= fromMaybeM (MerchantOperatingCityNotFound "merchantOpCityId-407c445a-2200-c45f-8d67-6f6dbfa28e73")
      Just city ->
        CQMOC.findByMerchantIdAndCity mId city
          >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> mId.getId <> "-city-" <> show city)
  let platformType = fromMaybe (DIBC.APPLICATION) _platformType
  integratedBPPConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) platformType

  allPossibleEndStops <- SIBC.fetchAllIntegratedBPPConfigResult integratedBPPConfigs $ \integratedBPPConfig -> do
    endStopsForAllStarts <- mapM (getPossibleEndStopsForStartStation integratedBPPConfig) req.stationCodes
    return $ concat endStopsForAllStarts

  let uniqueEndStops = nubBy (\a b -> a.code == b.code) allPossibleEndStops
  return uniqueEndStops
  where
    getPossibleEndStopsForStartStation :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSStationAPI]
    getPossibleEndStopsForStartStation integratedBPPConfig startStationCode = do
      currentTime <- getCurrentTime
      routesWithStop <- OTPRest.getRouteStopMappingByStopCode startStationCode integratedBPPConfig
      let routeCodes = nub $ map (.routeCode) routesWithStop
      routeStops <- EulerHS.Prelude.concatMapM (\routeCode -> OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig) routeCodes
      let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
          groupedStopsByRouteCode = groupBy (\a b -> a.routeCode == b.routeCode) $ sortBy (compare `on` (.routeCode)) serviceableStops
          possibleEndStops =
            groupBy (\a b -> a.stopCode == b.stopCode) $
              sortBy (compare `on` (.stopCode)) $
                concatMap
                  ( \stops ->
                      let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                       in sortBy (compare `on` (.sequenceNum)) $ filter (\stop -> maybe False (\startStopSequence -> stop.stopCode /= startStationCode && stop.sequenceNum > startStopSequence) mbStartStopSequence) stops
                  )
                  groupedStopsByRouteCode
      let endStops =
            concatMap
              ( \routeStops' ->
                  case routeStops' of
                    routeStop : xs ->
                      let routeCodes' = nub $ routeStop.routeCode : map (.routeCode) xs
                       in [ FRFSStationAPI
                              { name = Just routeStop.stopName,
                                code = routeStop.stopCode,
                                routeCodes = Just routeCodes',
                                lat = Just routeStop.stopPoint.lat,
                                lon = Just routeStop.stopPoint.lon,
                                integratedBppConfigId = integratedBPPConfig.id,
                                stationType = Nothing,
                                sequenceNum = Nothing,
                                address = Nothing,
                                distance = Nothing,
                                color = Nothing,
                                towards = Nothing,
                                timeTakenToTravelUpcomingStop = Nothing,
                                parentStopCode = Just startStationCode -- Set the parent stop code
                              }
                          ]
                    _ -> []
              )
              possibleEndStops
      return endStops

select :: CallExternalBPP.FRFSSelectFlow m r c => (FRFSCommon.DOnSelect -> Maybe Bool -> Maybe Bool -> m ()) -> Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSQuote.FRFSQuote -> [FRFSTicketService.FRFSCategorySelectionReq] -> Maybe Bool -> Maybe Bool -> m ()
select processOnSelectHandler merchant merchantOperatingCity bapConfig quote selectedQuoteCategories isSingleMode mbEnableOffer = do
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  updatedQuoteCategories <- updateQuoteCategoriesWithQuantitySelections (selectedQuoteCategories <&> (\category -> (category.quoteCategoryId, category.quantity))) quoteCategories
  CallExternalBPP.select processOnSelectHandler merchant merchantOperatingCity bapConfig quote updatedQuoteCategories isSingleMode mbEnableOffer
