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
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.FRFSConfig
import qualified Domain.Types.FRFSQuote as DFRFSQuote
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
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.RouteDetails as RD
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
import Kernel.Prelude hiding (whenJust)
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Common as Common
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common hiding (mkPrice)
import qualified Lib.JourneyLeg.Types as JLT
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
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
import qualified Storage.CachedQueries.RouteStopTimeTable as QRouteStopTimeTable
import qualified Storage.Queries.FRFSQuote as QFRFSQuote
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSSearch as QFRFSSearch
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingFeedback as QFRFSTicketBookingFeedback
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Maps as Maps
import qualified Tools.Payment as Payment

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
                                  integratedBppConfigId = integratedBPPConfig.id
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
    sequenceNum :: Maybe Int
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
        -- now <- getCurrentTime
        -- let currentTimeOfDay = utcToTimeOfDay now
        allSchedules <- QRouteStopTimeTable.findByRouteCodeAndStopCode integratedBPPConfig integratedBPPConfig.merchantId integratedBPPConfig.merchantOperatingCityId [route.code] (fromJust firstStop).stopCode
        -- TODO: Do we actually need to filter based on time. Timetable api we have for this.
        -- let futureSchedules = filter (\schedule -> schedule.timeOfDeparture > currentTimeOfDay) allSchedules
        --     sortedFutureSchedules = sortBy (compare `on` (.timeOfDeparture)) futureSchedules
        let frfsSchedule = listToMaybe allSchedules
        tripInfo' <- maybe (pure Nothing) (\schedule -> OTPRest.getNandiTripInfo integratedBPPConfig schedule.tripId.getId) frfsSchedule
        case tripInfo' of
          Just tripInfo -> do
            let stopSchedules = tripInfo.schedule
                stopInfos = tripInfo.stops
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
                                let currentDepartureTime = secondsToTimeOfDay' currentSchedule.departureTime
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
                              integratedBppConfigId = integratedBPPConfig.id
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
                        timeTakenToTravelUpcomingStop = Nothing
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
                        timeTakenToTravelUpcomingStop = Nothing
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
                        timeTakenToTravelUpcomingStop = Nothing
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
                                  timeTakenToTravelUpcomingStop = Nothing
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
              endStationCode = req.toStationCode
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
  postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ req frfsRouteDetails Nothing Nothing Nothing [] Nothing

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
  Maybe (Id DJL.JourneyLeg) ->
  [RD.RouteDetails] ->
  Maybe HighPrecMoney ->
  m API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
postFrfsSearchHandler (personId, merchantId) merchantOperatingCity integratedBPPConfig vehicleType_ FRFSSearchAPIReq {..} frfsRouteDetails mbPOrgTxnId mbPOrgId mbJourneyLegId mbRouteDetails mbFare = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory vehicleType_) >>= fromMaybeM (InternalError "Beckn Config not found")
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
            journeyLegInfo = journeySearchData,
            journeyLegStatus = Just JLT.InPlan,
            journeyRouteDetails = mbRouteDetails,
            journeyLegId = mbJourneyLegId,
            integratedBppConfigId = integratedBPPConfig.id,
            isOnSearchReceived = Nothing,
            validTill = Just validTill,
            ..
          }
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
        let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
            discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
        return $
          FRFSTicketService.FRFSQuoteAPIRes
            { quoteId = quote.id,
              _type = quote._type,
              price = quote.price.amount,
              priceWithCurrency = mkPriceAPIEntity quote.price,
              quantity = quote.quantity,
              validTill = quote.validTill,
              vehicleType = quote.vehicleType,
              discountedTickets = quote.discountedTickets,
              eventDiscountAmount = quote.eventDiscountAmount,
              integratedBppConfigId = quote.integratedBppConfigId,
              ..
            }
    )
    quotes

postFrfsQuoteV2Confirm :: CallExternalBPP.FRFSConfirmFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId req = do
  postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quoteId req Nothing

postFrfsQuoteV2ConfirmUtil :: CallExternalBPP.FRFSConfirmFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq -> Maybe MultimodalConfirm.CrisSdkResponse -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteV2ConfirmUtil (mbPersonId, merchantId_) quoteId req crisSdkResponse = do
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  quote <- B.runInReplica $ QFRFSQuote.findById quoteId >>= fromMaybeM (InvalidRequest "Invalid quote id")
  mbBooking <- QFRFSTicketBooking.findBySearchId quote.searchId
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity quote
  let isMultiInitAllowed =
        case mbBooking of
          Just booking ->
            case integratedBppConfig.providerConfig of
              DIBC.ONDC DIBC.ONDCBecknConfig {multiInitAllowed} ->
                multiInitAllowed == Just True
                  && booking.status `elem` [DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
              _ -> booking.status `elem` [DFRFSTicketBooking.APPROVED, DFRFSTicketBooking.PAYMENT_PENDING]
          Nothing -> True
  (rider, dConfirmRes) <- confirm isMultiInitAllowed quote mbBooking
  merchantOperatingCity <- getMerchantOperatingCityFromBooking dConfirmRes
  stations <- decodeFromText dConfirmRes.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< dConfirmRes.routeStationsJson
      discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< dConfirmRes.discountsJson
  now <- getCurrentTime
  when (isMultiInitAllowed && dConfirmRes.validTill > now) $ do
    bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory dConfirmRes.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
    let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
    mRiderNumber <- mapM decrypt rider.mobileNumber
    -- Add default TTL of 30 seconds or the value provided in the config
    let validTill = addUTCTime (maybe 30 intToNominalDiffTime bapConfig.initTTLSec) now
    void $ QFRFSTicketBooking.updateValidTillById validTill dConfirmRes.id
    let dConfirmRes' = dConfirmRes {DFRFSTicketBooking.validTill = validTill}
    when (dConfirmRes.status /= DFRFSTicketBooking.NEW) $ do
      void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.NEW dConfirmRes.id
    CallExternalBPP.init merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) dConfirmRes'
  return $ makeBookingStatusAPI dConfirmRes discounts routeStations stations merchantOperatingCity.city
  where
    -- errHandler booking exc
    --   | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelFRFSTicketBooking booking
    --   | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelFRFSTicketBooking booking
    --   | otherwise = throwM exc
    confirm :: CallExternalBPP.FRFSConfirmFlow m r => Bool -> DFRFSQuote.FRFSQuote -> Maybe DFRFSTicketBooking.FRFSTicketBooking -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    confirm isMultiInitAllowed quote mbBooking = do
      personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
      rider <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      let ticketQuantity = fromMaybe quote.quantity req.ticketQuantity
          childTicketQuantity = req.childTicketQuantity <|> quote.childTicketQuantity
      updatedQuote <-
        if isMultiInitAllowed
          then do
            void $ QFRFSQuote.updateTicketAndChildTicketQuantityById quoteId (Just ticketQuantity) childTicketQuantity
            return $ quote {DFRFSQuote.quantity = ticketQuantity, DFRFSQuote.childTicketQuantity = childTicketQuantity}
          else return quote
      unless (personId == quote.riderId) $ throwError AccessDenied
      let discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
      selectedDiscounts <- validateDiscounts req.discounts (fromMaybe [] discounts)

      now <- getCurrentTime
      unless (quote.validTill > now) $ throwError $ InvalidRequest "Quote expired"
      maybeM
        (buildAndCreateBooking rider updatedQuote selectedDiscounts)
        ( \booking -> do
            updatedBooking <-
              if isMultiInitAllowed
                then do
                  let mBookAuthCode = crisSdkResponse <&> (.bookAuthCode)
                  void $ QFRFSTicketBooking.updateBookingAuthCodeById mBookAuthCode booking.id
                  void $ QFRFSTicketBooking.updateQuoteAndBppItemId updatedQuote.id updatedQuote.bppItemId booking.id
                  let totalDiscount =
                        foldr
                          (\selectedDiscount discountAmount -> discountAmount + selectedDiscount.price.amount)
                          (HighPrecMoney 0.0)
                          selectedDiscounts
                  let childPriceAmount = maybe updatedQuote.price.amount (.amount) updatedQuote.childPrice
                  let discountedPrice = modifyPrice updatedQuote.price $ \p -> max (HighPrecMoney 0.0) $ HighPrecMoney ((p.getHighPrecMoney * (toRational updatedQuote.quantity)) + (childPriceAmount.getHighPrecMoney * (toRational (fromMaybe 0 updatedQuote.childTicketQuantity)))) - totalDiscount
                  void $ QFRFSTicketBooking.updatePriceAndQuantityById discountedPrice updatedQuote.quantity updatedQuote.childTicketQuantity booking.id
                  return $ booking {DFRFSTicketBooking.quoteId = quoteId, DFRFSTicketBooking.bppItemId = updatedQuote.bppItemId, DFRFSTicketBooking.bookingAuthCode = mBookAuthCode, DFRFSTicketBooking.price = discountedPrice, DFRFSTicketBooking.quantity = ticketQuantity, DFRFSTicketBooking.childTicketQuantity = childTicketQuantity}
                else return booking
            pure (rider, updatedBooking)
        )
        (pure mbBooking)

    validateDiscounts :: (MonadFlow m) => [FRFSDiscountReq] -> [FRFSDiscountRes] -> m [FRFSDiscountRes]
    validateDiscounts selectedDiscounts allDiscounts = do
      let selecetedDiscountCodes = map (.code) selectedDiscounts
          eligibleDiscountCodes = map (.code) $ filter (.eligibility) allDiscounts
      unless (all (flip elem eligibleDiscountCodes) selecetedDiscountCodes) $ throwError DiscountsIneligible
      return $ intersectBy (\discount selectedDiscount -> discount.code == selectedDiscount.code) allDiscounts selectedDiscounts
      where
        intersectBy :: (a -> b -> Bool) -> [a] -> [b] -> [a]
        intersectBy _ [] _ = []
        intersectBy _ _ [] = []
        intersectBy f as bs = filter (\a -> any (f a) bs) as

    buildAndCreateBooking :: CallExternalBPP.FRFSConfirmFlow m r => Domain.Types.Person.Person -> DFRFSQuote.FRFSQuote -> [FRFSDiscountRes] -> m (Domain.Types.Person.Person, DFRFSTicketBooking.FRFSTicketBooking)
    buildAndCreateBooking rider quote@DFRFSQuote.FRFSQuote {..} selectedDiscounts = do
      uuid <- generateGUID
      now <- getCurrentTime
      mbSearch <- QFRFSSearch.findById searchId
      let appliedDiscountsJson = encodeToText selectedDiscounts
          totalDiscount =
            foldr
              (\selectedDiscount discountAmount -> discountAmount + selectedDiscount.price.amount)
              (HighPrecMoney 0.0)
              selectedDiscounts
      let ticketQuantity' = quote.quantity
      let childTicketQuantity' = fromMaybe 0 childTicketQuantity
      let childPriceAmount = maybe quote.price.amount (.amount) quote.childPrice
      let discountedPrice = modifyPrice quote.price $ \p -> max (HighPrecMoney 0.0) $ HighPrecMoney ((p.getHighPrecMoney * (toRational ticketQuantity')) + (childPriceAmount.getHighPrecMoney * (toRational childTicketQuantity'))) - totalDiscount
      let isFareChanged = if isJust partnerOrgId then isJust oldCacheDump else maybe False (\estimatedPrice' -> quote.price /= estimatedPrice') quote.estimatedPrice
      let journeyRouteDetails' = maybe [] (.journeyRouteDetails) mbSearch
      let booking =
            DFRFSTicketBooking.FRFSTicketBooking
              { id = uuid,
                bppOrderId = Nothing,
                quoteId = id,
                status = DFRFSTicketBooking.NEW,
                createdAt = now,
                updatedAt = now,
                merchantId = quote.merchantId,
                price = discountedPrice,
                estimatedPrice = discountedPrice,
                finalPrice = Nothing, -- TODO :: Analytics Kind of Pricing, not sent to UI
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
                discountsJson = Just appliedDiscountsJson,
                journeyLegOrder = mbSearch >>= (.journeyLegInfo) <&> (.journeyLegOrder),
                journeyId = Id <$> (mbSearch >>= (.journeyLegInfo) <&> (.journeyId)),
                journeyOnInitDone = Nothing,
                journeyLegStatus = mbSearch >>= (.journeyLegStatus),
                journeyRouteDetails = journeyRouteDetails',
                startTime = Just now, -- TODO
                isFareChanged = Just isFareChanged,
                integratedBppConfigId = quote.integratedBppConfigId,
                googleWalletJWTUrl = Nothing,
                isDeleted = Just False,
                isSkipped = Just False,
                quantity = ticketQuantity',
                childTicketQuantity,
                bookingAuthCode = crisSdkResponse <&> (.bookAuthCode),
                osType = crisSdkResponse <&> (.osType),
                osBuildVersion = crisSdkResponse <&> (.osBuildVersion),
                recentLocationId = mbSearch >>= (.recentLocationId),
                journeyLegId = mbSearch >>= (.journeyLegId),
                ..
              }
      QFRFSTicketBooking.create booking
      return (rider, booking)

    makeBookingStatusAPI booking discounts routeStations stations city =
      FRFSTicketService.FRFSTicketBookingStatusAPIRes
        { bookingId = booking.id,
          city,
          updatedAt = booking.updatedAt,
          createdAt = booking.createdAt,
          _type = booking._type,
          price = booking.price.amount,
          priceWithCurrency = mkPriceAPIEntity booking.price,
          quantity = booking.quantity,
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

postFrfsQuoteConfirm :: CallExternalBPP.FRFSConfirmFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> m API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm (mbPersonId, merchantId_) quoteId = do
  postFrfsQuoteV2Confirm (mbPersonId, merchantId_) quoteId (API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq {discounts = [], ticketQuantity = Nothing, childTicketQuantity = Nothing})

postFrfsQuotePaymentRetry :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSQuote.FRFSQuote -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry = error "Logic yet to be decided"

webhookHandlerFRFSTicket :: Kernel.Types.Id.ShortId DPaymentOrder.PaymentOrder -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Environment.Flow ()
webhookHandlerFRFSTicket paymentOrderId merchantId = do
  logDebug $ "frfs ticket order bap webhookc call" <> paymentOrderId.getShortId
  order <- QPaymentOrder.findByShortId paymentOrderId >>= fromMaybeM (PaymentOrderNotFound paymentOrderId.getShortId)
  bookingPayments <- QFRFSTicketBookingPayment.findAllByOrderId order.id
  bookings <- mapMaybeM (QFRFSTicketBooking.findById . (.frfsTicketBookingId)) bookingPayments

  mapM_ (\booking -> frfsBookingStatus (booking.riderId, merchantId) False booking) bookings

getFrfsBookingStatus :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus (mbPersonId, merchantId_) bookingId = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  booking <- B.runInReplica $ QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  frfsBookingStatus (personId, merchantId_) False booking

-- pass isMultiModalBooking = True in case of multimodal flow
frfsBookingStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Bool -> DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
frfsBookingStatus (personId, merchantId_) isMultiModalBooking booking' = do
  logInfo $ "frfsBookingStatus for booking: " <> show booking'
  let bookingId = booking'.id
  merchant <- CQM.findById merchantId_ >>= fromMaybeM (InvalidRequest "Invalid merchant id")
  bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback booking'.merchantOperatingCityId merchant.id (show Spec.FRFS) (frfsVehicleCategoryToBecknVehicleCategory booking'.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  unless (personId == booking'.riderId) $ throwError AccessDenied
  person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  now <- getCurrentTime
  let validTillWithBuffer = addUTCTime 5 booking'.validTill
  when (booking'.status /= DFRFSTicketBooking.CONFIRMED && booking'.status /= DFRFSTicketBooking.FAILED && booking'.status /= DFRFSTicketBooking.CANCELLED && validTillWithBuffer < now) $
    void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest "Invalid booking id")
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  let commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
  logInfo $ "Booking status: " <> show booking.status
  case booking.status of
    DFRFSTicketBooking.NEW -> buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.FAILED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking (Just person.id.getId) person.clientSdkVersion)
      logInfo $ "payment status resp: " <> show paymentStatusResp
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      logInfo $ "payment booking status: " <> show paymentBookingStatus
      when (paymentBookingStatus == FRFSTicketService.FAILURE) do
        void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
        let mPrice = Common.mkPrice (Just booking'.price.currency) (HighPrecMoney $ toRational (0 :: Int))
        void $ QFRFSRecon.updateTOrderValueAndSettlementAmountById mPrice mPrice booking.id
      when (paymentBookingStatus == FRFSTicketService.SUCCESS) do
        void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
        riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
        when riderConfig.enableAutoJourneyRefund $ refundOrderCall booking person
        markJourneyPaymentSuccess booking.journeyId paymentOrder
      when (paymentBookingStatus == FRFSTicketService.PENDING) do
        void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
        void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.PENDING booking.id
      let paymentStatusAPI =
            case paymentBookingStatus of
              FRFSTicketService.FAILURE -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.FAILED
              FRFSTicketService.SUCCESS -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.REFUND_PENDING
              FRFSTicketService.PENDING -> Just $ Utils.mkTBPStatusAPI DFRFSTicketBookingPayment.PENDING
              _ -> Nothing
      logInfo $ "payment status api: " <> show paymentStatusAPI
      let mbPaymentObj = paymentStatusAPI <&> \status -> FRFSTicketService.FRFSBookingPaymentAPI {status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.CONFIRMING -> do
      if addUTCTime 5 booking.validTill < now
        then do
          logInfo $ "booking is expired in confirming: " <> show booking
          void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING bookingId
          riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
          when riderConfig.enableAutoJourneyRefund $ refundOrderCall booking person
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else do
          buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.CONFIRMED -> do
      void $ CallExternalBPP.status merchant.id merchantOperatingCity bapConfig booking
      buildFRFSTicketBookingStatusAPIRes booking paymentSuccess
    DFRFSTicketBooking.APPROVED -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking (Just person.id.getId) person.clientSdkVersion)
      logInfo $ "payment status response: " <> show paymentStatusResp
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      if paymentBookingStatus == FRFSTicketService.FAILURE
        then do
          logInfo $ "payment failed in approved: " <> show booking
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              logInfo $ "booking is expired in approved: " <> show booking
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
              when riderConfig.enableAutoJourneyRefund $ refundOrderCall booking person
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
            else do
              txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
              let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.PAYMENT_PENDING bookingId
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.PAYMENT_PENDING Nothing Nothing
              paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
              let paymentObj =
                    Just $
                      FRFSTicketService.FRFSBookingPaymentAPI
                        { status = paymentStatus_,
                          paymentOrder = paymentOrder_,
                          transactionId = Nothing
                        }
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentObj
    DFRFSTicketBooking.PAYMENT_PENDING -> do
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId bookingId >>= fromMaybeM (InvalidRequest "Payment booking not found for approved TicketBookingId")
      paymentOrder <- QPaymentOrder.findById paymentBooking.paymentOrderId >>= fromMaybeM (InvalidRequest "Payment order not found for approved TicketBookingId")
      paymentStatusResp <- DPayment.orderStatusService commonPersonId paymentOrder.id (orderStatusCall merchantOperatingCity.id booking (Just person.id.getId) person.clientSdkVersion)
      logInfo $ "paymentStatusResp: " <> show paymentStatusResp
      let paymentBookingStatus = makeTicketBookingPaymentAPIStatus paymentStatusResp.status
      logInfo $ "paymentBookingStatus: " <> show paymentBookingStatus
      if paymentBookingStatus == FRFSTicketService.FAILURE
        then do
          logInfo $ "payment failed in payment pending: " <> show booking
          QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED bookingId
          QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.FAILED booking.id
          let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
          buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
        else
          if (paymentBookingStatus == FRFSTicketService.SUCCESS) && (booking.validTill < now)
            then do
              logInfo $ "booking is expired in payment success and booking is expired: " <> show booking
              void $ QFRFSTicketBooking.updateStatusById DFRFSTicketBooking.FAILED booking.id
              void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.REFUND_PENDING booking.id
              riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCity.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCity.id.getId)
              when riderConfig.enableAutoJourneyRefund $ refundOrderCall booking person
              let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.FAILED Nothing Nothing
              buildFRFSTicketBookingStatusAPIRes updatedBooking paymentFailed
            else
              if paymentBookingStatus == FRFSTicketService.SUCCESS
                then do
                  -- Add default TTL of 1 min or the value provided in the config
                  let updatedTTL = addUTCTime (maybe 60 intToNominalDiffTime bapConfig.confirmTTLSec) now
                  transactions <- QPaymentTransaction.findAllByOrderId paymentOrder.id
                  txnId <- getSuccessTransactionId transactions
                  let updatedBooking = makeUpdatedBooking booking DFRFSTicketBooking.CONFIRMING (Just updatedTTL) (Just txnId.getId)
                  -- setNxExpire
                  isLockAcquired <- Hedis.tryLockRedis (mkPaymentSuccessLockKey bookingId) 60
                  when isLockAcquired $ do
                    void $ QFRFSTicketBookingPayment.updateStatusByTicketBookingId DFRFSTicketBookingPayment.SUCCESS booking.id
                    void $ QFRFSTicketBooking.updateStatusValidTillAndPaymentTxnById DFRFSTicketBooking.CONFIRMING updatedTTL (Just txnId.getId) booking.id
                    markJourneyPaymentSuccess booking.journeyId paymentOrder
                    let mRiderName = person.firstName <&> (\fName -> person.lastName & maybe fName (\lName -> fName <> " " <> lName))
                    mRiderNumber <- mapM decrypt person.mobileNumber
                    void $ QFRFSTicketBooking.insertPayerVpaIfNotPresent paymentStatusResp.payerVpa bookingId
                    void $ CallExternalBPP.confirm processOnConfirm merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) updatedBooking
                    when isMultiModalBooking do
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
                  buildFRFSTicketBookingStatusAPIRes updatedBooking paymentSuccess
                else do
                  logInfo $ "payment success in payment pending: " <> show booking
                  paymentOrder_ <- buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCity.id booking
                  txn <- QPaymentTransaction.findNewTransactionByOrderId paymentOrder.id
                  let paymentStatus_ = if isNothing txn then FRFSTicketService.NEW else paymentBookingStatus
                      paymentObj =
                        Just
                          FRFSTicketService.FRFSBookingPaymentAPI
                            { status = paymentStatus_,
                              paymentOrder = paymentOrder_,
                              transactionId = Nothing
                            }
                  buildFRFSTicketBookingStatusAPIRes booking paymentObj
    DFRFSTicketBooking.CANCELLED -> do
      FRFSCommon.updateTotalOrderValueAndSettlementAmount booking bapConfig
      paymentBooking <- B.runInReplica $ QFRFSTicketBookingPayment.findNewTBPByBookingId booking.id
      let mbPaymentObj = paymentBooking <&> \tbp -> FRFSTicketService.FRFSBookingPaymentAPI {status = Utils.mkTBPStatusAPI tbp.status, paymentOrder = Nothing, transactionId = Nothing}
      buildFRFSTicketBookingStatusAPIRes booking mbPaymentObj
    DFRFSTicketBooking.COUNTER_CANCELLED -> do
      FRFSCommon.updateTotalOrderValueAndSettlementAmount booking bapConfig
      buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.CANCEL_INITIATED -> do
      buildFRFSTicketBookingStatusAPIRes booking Nothing
    DFRFSTicketBooking.TECHNICAL_CANCEL_REJECTED -> do
      buildFRFSTicketBookingStatusAPIRes booking Nothing
  where
    markJourneyPaymentSuccess mbJourneyId paymentOrder = do
      whenJust mbJourneyId $ \journeyId -> do
        void $ QJourney.updatePaymentOrderShortId (Just paymentOrder.shortId) (Just True) journeyId
        void $ QJourney.updateStatus DJ.INPROGRESS journeyId

    mkPaymentSuccessLockKey bookingId = "frfsPaymentSuccess:" <> bookingId.getId

    paymentSuccess =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.SUCCESS,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    processOnConfirm onConfirmReq = do
      (merchant', booking'') <- DACFOC.validateRequest onConfirmReq
      DACFOC.onConfirm merchant' booking'' onConfirmReq

    paymentFailed =
      Just $
        FRFSTicketService.FRFSBookingPaymentAPI
          { status = FRFSTicketService.FAILURE,
            paymentOrder = Nothing,
            transactionId = Nothing
          }

    buildCreateOrderResp paymentOrder person commonPersonId merchantOperatingCityId booking = do
      personEmail <- mapM decrypt person.email
      personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
      isSplitEnabled_ <- Payment.getIsSplitEnabled merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType)
      let createOrderReq =
            Payment.CreateOrderReq
              { orderId = paymentOrder.id.getId,
                orderShortId = paymentOrder.shortId.getShortId,
                amount = paymentOrder.amount,
                customerId = person.id.getId,
                customerEmail = fromMaybe "test@gmail.com" personEmail,
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
                splitSettlementDetails = Payment.mkSplitSettlementDetails isSplitEnabled_ paymentOrder.amount []
              }
      DPayment.createOrderService commonMerchantId (Just $ cast merchantOperatingCityId) commonPersonId createOrderReq (createOrderCall merchantOperatingCityId booking (Just person.id.getId) person.clientSdkVersion)

    getPaymentType = \case
      Spec.METRO -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBooking
      Spec.SUBWAY -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBooking
      Spec.BUS -> if isMultiModalBooking then Payment.FRFSMultiModalBooking else Payment.FRFSBusBooking

    createOrderCall merchantOperatingCityId booking mRoutingId sdkVersion = Payment.createOrder merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType) mRoutingId sdkVersion
    orderStatusCall merchantOperatingCityId booking mRoutingId sdkVersion = Payment.orderStatus merchantId_ merchantOperatingCityId Nothing (getPaymentType booking.vehicleType) mRoutingId sdkVersion
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
    refundOrderCall booking person = do
      logInfo $ "payment status api refundOrderCall"
      riderConfig <- QRC.findByMerchantOperatingCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
      when riderConfig.enableAutoJourneyRefund $
        FRFSUtils.markAllRefundBookings booking person.id

getFrfsBookingList :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Maybe Spec.VehicleCategory -> Environment.Flow [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
getFrfsBookingList (mbPersonId, merchantId) mbLimit mbOffset mbVehicleCategory = do
  personId <- fromMaybeM (InvalidRequest "Invalid person id") mbPersonId
  bookings <- B.runInReplica $ QFRFSTicketBooking.findAllByRiderId mbLimit mbOffset personId mbVehicleCategory
  case mbVehicleCategory of
    Just Spec.BUS -> mapM (frfsBookingStatus (personId, merchantId) False) bookings
    _ -> mapM (`buildFRFSTicketBookingStatusAPIRes` Nothing) bookings

buildFRFSTicketBookingStatusAPIRes :: DFRFSTicketBooking.FRFSTicketBooking -> Maybe FRFSTicketService.FRFSBookingPaymentAPI -> Environment.Flow FRFSTicketService.FRFSTicketBookingStatusAPIRes
buildFRFSTicketBookingStatusAPIRes booking payment = do
  integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
  stations <- mapM (Utils.mkPOrgStationAPI booking.partnerOrgId integratedBppConfig) =<< (decodeFromText booking.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db"))
  let routeStations :: Maybe [FRFSRouteStationsAPI] = decodeFromText =<< booking.routeStationsJson
      discounts :: Maybe [FRFSDiscountRes] = decodeFromText =<< booking.discountsJson
  merchantOperatingCity <- getMerchantOperatingCityFromBooking booking
  tickets' <- B.runInReplica $ QFRFSTicket.findAllByTicketBookingId booking.id
  let tickets =
        map
          ( \DFRFSTicket.FRFSTicket {..} ->
              FRFSTicketService.FRFSTicketAPI {..}
          )
          tickets'
  return $
    FRFSTicketService.FRFSTicketBookingStatusAPIRes
      { bookingId = booking.id,
        city = merchantOperatingCity.city,
        updatedAt = booking.updatedAt,
        createdAt = booking.createdAt,
        _type = booking._type,
        price = booking.price.amount,
        priceWithCurrency = mkPriceAPIEntity booking.price,
        quantity = booking.quantity,
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

makeTicketBookingPaymentAPIStatus :: Payment.TransactionStatus -> FRFSTicketService.FRFSBookingPaymentStatusAPI
makeTicketBookingPaymentAPIStatus Payment.NEW = FRFSTicketService.NEW
makeTicketBookingPaymentAPIStatus PENDING_VBV = FRFSTicketService.PENDING
makeTicketBookingPaymentAPIStatus CHARGED = FRFSTicketService.SUCCESS
makeTicketBookingPaymentAPIStatus AUTHENTICATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus AUTHORIZATION_FAILED = FRFSTicketService.PENDING -- FRFSTicketService.FAILURE
makeTicketBookingPaymentAPIStatus JUSPAY_DECLINED = FRFSTicketService.FAILURE
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
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
  unless (ticketBooking.status == DFRFSTicketBooking.CONFIRMED) $ throwError (InvalidRequest "Cancellation during incorrect status")
  -- tickets <- QFRFSTicket.findAllByTicketBookingId ticketBooking.id
  -- unless (all (\ticket -> ticket.status == DFRFSTicket.ACTIVE) tickets) $ throwError (InvalidRequest "Cancellation during incorrect status")
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
  frfsConfig <-
    CQFRFSConfig.findByMerchantOperatingCityIdInRideFlow ticketBooking.merchantOperatingCityId []
      >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show ticketBooking.merchantOperatingCityId)
  unless (frfsConfig.isCancellationAllowed) $ throwError CancellationNotSupported
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
                      sequenceNum = stop.sequenceNum
                    }
              )
              stops

      let maxBatchSize = 100
          stopBatches = chunksOf maxBatchSize transformedStops

      batchedResults <- fmap concat $
        forM stopBatches $ \batch -> do
          res <-
            try @_ @SomeException $
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
