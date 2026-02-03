module Domain.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService
import qualified API.Types.UI.FRFSTicketService as FRFSTicketService
import BecknV2.FRFS.Enums hiding (END, START)
import qualified BecknV2.FRFS.Enums as Spec
import BecknV2.FRFS.Utils
import Control.Monad.Extra hiding (fromMaybeM)
import qualified Data.HashMap.Strict as HashMap
import Data.List (groupBy, nub, nubBy)
import qualified Data.List.NonEmpty as NonEmpty hiding (groupBy, map, nub, nubBy)
import Data.List.Split (chunksOf)
import qualified Data.Time as Time
import Domain.Types.BecknConfig
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingFeedback as DFRFSTicketBookingFeedback
import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig as DIBC
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
import EulerHS.Prelude hiding (all, and, any, concatMap, elem, find, foldr, forM_, fromList, groupBy, hoistMaybe, id, length, map, mapM_, maximum, null, readMaybe, toList, whenJust)
import qualified ExternalBPP.CallAPI.Cancel as CallExternalBPP
import qualified ExternalBPP.CallAPI.Search as CallExternalBPP
import qualified ExternalBPP.CallAPI.Select as CallExternalBPP
import qualified ExternalBPP.CallAPI.Types as CallExternalBPP
import qualified ExternalBPP.CallAPI.Verify as CallExternalBPP
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps.Interface.Types
import qualified Kernel.External.Maps.Types
import Kernel.External.MultiModal.Utils
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (whenJust)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import SharedLogic.External.Nandi.Types (StopInfo (..), StopSchedule (..))
import SharedLogic.FRFSConfirm
import SharedLogic.FRFSStatus
import SharedLogic.FRFSUtils
import SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
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
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingFeedback as QFRFSTicketBookingFeedback
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Maps as Maps
import Tools.Metrics.BAPMetrics (HasBAPMetrics)
import qualified Tools.Payment as Payment
import qualified Tools.Wallet as TWallet
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
  let platformType = fromMaybe DIBC.APPLICATION _platformType
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
              serviceTier = req.serviceTier
            }
        ]
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  let platformType = fromMaybe DIBC.APPLICATION req.platformType
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
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) platformType
  postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ req frfsRouteDetails Nothing Nothing Nothing Nothing (\_ -> pure ()) [] [] -- the journey leg upsert function is not required here

postFrfsDiscoverySearch :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) -> API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postFrfsDiscoverySearch (_, merchantId) mbIntegratedBPPConfigId req = do
  let platformType = fromMaybe DIBC.APPLICATION req.platformType
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchantId req.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchantId.getId <> " ,city: " <> show req.city)
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOpCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory req.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId merchantOpCity.id (frfsVehicleCategoryToBecknVehicleCategory req.vehicleType) platformType
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
  [Spec.ServiceTierType] ->
  [DFRFSQuote.FRFSQuoteType] ->
  m API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ FRFSSearchAPIReq {..} frfsRouteDetails mbPOrgTxnId mbPOrgId mbFare multimodalSearchRequestId upsertJourneyLegAction blacklistedServiceTiers blacklistedFareQuoteTypes = do
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
            fromStationPoint = LatLong <$> fromStation.lat <*> fromStation.lon,
            toStationPoint = LatLong <$> toStation.lat <*> toStation.lon,
            fromStationName = Just fromStation.name,
            toStationName = Just toStation.name,
            fromStationAddress = fromStation.address,
            toStationAddress = toStation.address,
            routeCode = route <&> (.code),
            riderId = personId,
            partnerOrgTransactionId = mbPOrgTxnId,
            partnerOrgId = mbPOrgId,
            integratedBppConfigId = integratedBPPConfig.id,
            isOnSearchReceived = Nothing,
            onSearchFailed = Nothing,
            validTill = Just validTill,
            searchAsParentStops = searchAsParentStops,
            busLocationData = fromMaybe [] busLocationData,
            ..
          }
  upsertJourneyLegAction searchReqId.getId
  QFRFSSearch.create searchReq
  CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq mbFare frfsRouteDetails integratedBPPConfig blacklistedServiceTiers blacklistedFareQuoteTypes
  quotes <-
    withTryCatch "getFrfsSearchQuote" (getFrfsSearchQuote (Just personId, merchantId) searchReqId)
      >>= \case
        Right frfsQuotes -> return frfsQuotes
        Left _ -> return []
  return $ FRFSSearchAPIRes quotes searchReqId

getFrfsSearchQuote :: (CallExternalBPP.FRFSSearchFlow m r, HasShortDurationRetryCfg r c) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
getFrfsSearchQuote (mbPersonId, _) searchId_ = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  search <- QFRFSSearch.findById searchId_ >>= fromMaybeM (InvalidRequest "Invalid search id")
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity search
  unless (personId == search.riderId) $ throwError AccessDenied
  (quotes :: [DFRFSQuote.FRFSQuote]) <- B.runInReplica $ QFRFSQuote.findAllBySearchId searchId_
  quotesWithCategories <- mapM (\quote -> (quote,) <$> QFRFSQuoteCategory.findAllByQuoteId quote.id) quotes
  sortedQuotesWithCategories <- case search.vehicleType of
    Spec.BUS -> do
      mbRiderConfig <- QRC.findByMerchantOperatingCityId search.merchantOperatingCityId Nothing
      let cfgMap = maybe (JourneyUtils.toCfgMap JourneyUtils.defaultBusTierSortingConfig) JourneyUtils.toCfgMap (mbRiderConfig >>= (.busTierSortingConfig))
          serviceTierTypeFromQuote quote quoteCategories = JourneyUtils.getServiceTierFromQuote quoteCategories quote <&> (.serviceTierType)
      return $
        sortBy
          ( \(quote1, quoteCategories1) (quote2, quoteCategories2) ->
              compare
                (maybe maxBound (JourneyUtils.tierRank cfgMap) (serviceTierTypeFromQuote quote1 quoteCategories1))
                (maybe maxBound (JourneyUtils.tierRank cfgMap) (serviceTierTypeFromQuote quote2 quoteCategories2))
          )
          quotesWithCategories
    _ ->
      return $
        sortBy
          ( \(_, quoteCategories1) (_, quoteCategories2) ->
              let mbAdultPrice1 = find (\category -> category.category == ADULT) quoteCategories1 <&> (.price)
                  mbAdultPrice2 = find (\category -> category.category == ADULT) quoteCategories2 <&> (.price)
               in compare (maybe 0 (.amount) mbAdultPrice1) (maybe 0 (.amount) mbAdultPrice2)
          )
          quotesWithCategories
  mapM
    ( \(quote, quoteCategories) -> do
        let (routeStations :: Maybe [FRFSRouteStationsAPI], stations :: Maybe [FRFSStationAPI]) =
              if integratedBppConfig.platformType == DIBC.MULTIMODAL
                then (Nothing, Nothing)
                else (decodeFromText =<< quote.routeStationsJson, decodeFromText quote.stationsJson)
        let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
            categories = map mkCategoryInfoResponse quoteCategories
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
              stations = fromMaybe [] stations,
              ..
            }
    )
    sortedQuotesWithCategories

postFrfsQuoteV2Confirm :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Maybe Bool -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2Confirm (mbPersonId, merchantId) quoteId mbIsMockPayment req = do
  personId <- fromMaybeM (InvalidRequest "personId not found") mbPersonId
  selectedQuoteCategories <-
    case req.offered of
      Just offeredCategories
        | not (null offeredCategories) ->
          pure $
            map
              ( \offeredCategory ->
                  FRFSTicketService.FRFSCategorySelectionReq
                    { quoteCategoryId = offeredCategory.quoteCategoryId,
                      quantity = offeredCategory.quantity
                    }
              )
              offeredCategories
      _ -> do
        quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quoteId
        pure $
          map
            ( \quoteCategory ->
                let quantity =
                      fromMaybe quoteCategory.selectedQuantity
                        case quoteCategory.category of
                          ADULT -> req.ticketQuantity
                          _ -> Just quoteCategory.selectedQuantity
                 in FRFSTicketService.FRFSCategorySelectionReq {quoteCategoryId = quoteCategory.id, quantity}
            )
            quoteCategories

  quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  case integratedBppConfig.providerConfig of
    DIBC.ONDC _ | quote.vehicleType == Spec.BUS -> do
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
      merchantOperatingCity <- CQMOC.findById quote.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound quote.merchantOperatingCityId.getId)
      bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory quote.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
      (_, booking, _, _, _) <- confirmAndUpsertBooking personId quote selectedQuoteCategories req.crisSdkResponse (Just True) mbIsMockPayment integratedBppConfig
      select merchant merchantOperatingCity bapConfig quote selectedQuoteCategories req.crisSdkResponse (Just True) req.enableOffer
      getFrfsBookingStatus (Just personId, merchantId) booking.id
    _ -> do
      postFrfsQuoteV2ConfirmUtil (Just personId, merchantId) quote selectedQuoteCategories req.crisSdkResponse (Just True) req.enableOffer mbIsMockPayment integratedBppConfig

postFrfsQuoteConfirm :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Maybe Bool -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId mbIsMockPayment = do
  postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId mbIsMockPayment (API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq {offered = Nothing, ticketQuantity = Nothing, childTicketQuantity = Nothing, crisSdkResponse = Nothing, enableOffer = Nothing})

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

frfsOrderStatusHandler ::
  (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) =>
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
          integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
          journeyId <- getJourneyIdFromBooking booking
          bookingStatus <- frfsBookingStatus (booking.riderId, merchantId) (integratedBppConfig.platformType == DIBC.MULTIMODAL) (withPaymentStatusResponseHandler bookingPayment paymentOrder) booking person switchFRFSQuoteTier
          return (bookingStatus, booking, journeyId)
      )
      bookingPayments
  let (bookingsStatus, _, journeyIds) = unzip3 bookingsStatusWithBooking
      journeyId = listToMaybe $ catMaybes journeyIds
  return $
    ( evaluateConditions
        [ (Just DFRFSTicketBooking.CONFIRMED, Nothing, DPayment.FulfillmentSucceeded, all), -- Ticket Generated
          (Nothing, Just FRFSTicketService.REFUND_PENDING, DPayment.FulfillmentRefundPending, all), -- Paid But Refund Pending (Could be due to Booking Cancellation/Failure/Async Ticket Generation Failure)
          (Nothing, Just FRFSTicketService.REFUND_INITIATED, DPayment.FulfillmentRefundInitiated, all), -- Paid But Refund Initiated (Could be due to Booking Cancellation/Failure/Async Ticket Generation Failure)
          (Nothing, Just FRFSTicketService.REFUND_FAILED, DPayment.FulfillmentRefundFailed, all), -- Paid But Refund Failed (Could be due to Booking Cancellation/Failure/Async Ticket Generation Failure)
          (Nothing, Just FRFSTicketService.REFUNDED, DPayment.FulfillmentRefunded, all), -- Paid But Refunded (Could be due to Booking Cancellation/Failure/Async Ticket Generation Failure/Auto Refund)
          (Just DFRFSTicketBooking.FAILED, Nothing, DPayment.FulfillmentFailed, any) -- If Any Booking Failed, fulfillment cannot happen and should be marked as Failed
        ]
        bookingsStatus,
      journeyId <&> (.getId),
      Nothing
    )
  where
    -- evaluateConditions :: Foldable t => [(Maybe DFRFSTicketBooking.FRFSTicketBookingStatus, Maybe FRFSTicketService.FRFSBookingPaymentStatusAPI, Payment.PaymentFulfillmentStatus, ((a -> Bool) -> t a -> Bool))] -> [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes] -> Payment.PaymentFulfillmentStatus
    evaluateConditions _ [] = DPayment.FulfillmentPending
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
      ( (DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, Maybe DPayment.PaymentStatusResp) ->
        m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      ) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler paymentBooking paymentOrder action = action (paymentBooking, paymentOrder, Just paymentStatusResponse)

getFrfsBookingStatus ::
  (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking ->
  m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  frfsBookingStatus (personId, merchantId_) (integratedBppConfig.platformType == DIBC.MULTIMODAL) (withPaymentStatusResponseHandler integratedBppConfig booking person) booking person (\_ _ -> pure ())
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
      DIBC.IntegratedBPPConfig ->
      DFRFSTicketBooking.FRFSTicketBooking ->
      DP.Person ->
      ((DFRFSTicketBookingPayment.FRFSTicketBookingPayment, DPaymentOrder.PaymentOrder, Maybe DPayment.PaymentStatusResp) -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes) ->
      m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
    withPaymentStatusResponseHandler integratedBppConfig booking person action = do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findTicketBookingPayment booking >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person booking.riderId
      let orderStatusCall = Payment.orderStatus booking.merchantId booking.merchantOperatingCityId Nothing (getPaymentType (integratedBppConfig.platformType == DIBC.MULTIMODAL) booking.vehicleType) (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
      paymentStatusResponse <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
      action (paymentBooking, paymentOrder, Just paymentStatusResponse)

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
  let platformType = fromMaybe DIBC.APPLICATION _platformType
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
  let platformType = fromMaybe DIBC.APPLICATION _platformType
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
  let platformType = fromMaybe DIBC.APPLICATION _platformType
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

select :: (CallExternalBPP.FRFSConfirmFlow m r c, HasField "blackListedJobs" r [Text]) => Merchant -> MerchantOperatingCity -> BecknConfig -> DFRFSQuote.FRFSQuote -> [FRFSTicketService.FRFSCategorySelectionReq] -> Maybe CrisSdkResponse -> Maybe Bool -> Maybe Bool -> m ()
select merchant merchantOperatingCity bapConfig quote selectedQuoteCategories crisSdkResponse isSingleMode mbEnableOffer = do
  quoteCategories <- QFRFSQuoteCategory.findAllByQuoteId quote.id
  updatedQuoteCategories <- updateQuoteCategoriesWithQuantitySelections (selectedQuoteCategories <&> (\category -> (category.quoteCategoryId, category.quantity))) quoteCategories
  CallExternalBPP.select merchant merchantOperatingCity bapConfig quote updatedQuoteCategories crisSdkResponse isSingleMode mbEnableOffer
