module Domain.Action.UI.FRFSFleetOperator
  ( getV2FrfsRoute,
    getV2FrfsTripRouteManifest,
    postFrfsFleetOperatorTripAction,
    postFrfsFleetOperatorCurrentOperation,
  )
where

import API.Types.UI.FRFSFleetOperator
import BecknV2.FRFS.Enums (VehicleCategory (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (unpack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.FleetOperatorTripAction (FleetOperatorTripAction (..))
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Environment (Flow)
import EulerHS.Prelude hiding (id, unpack)
import Kernel.Prelude (listToMaybe)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (Seconds (..))
import Kernel.Types.Id (Id (..), getId)
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common (fork, fromMaybeM, getCurrentTime, logError, logInfo, throwError)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.CallBAPInternal (getFrfsTripManifest, notifyFrfsTripStarted)
import SharedLogic.IntegratedBPPConfig (findFirstIbppConfigByCityAndVehicle, findIntegratedBPPConfig, getGimsBaseUrl)
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error (GenericError (InvalidRequest))

getV2FrfsRoute ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Kernel.Types.Beckn.Context.City ->
    VehicleCategory ->
    Flow FRFSRouteAPI
  )
getV2FrfsRoute (_, _merchantId, merchantOpCityId) routeCode mbConfigId mbPlatformType _city vehicleType = do
  logInfo $ "FRFSFleetOperator: Fetching route for routeCode: " <> routeCode

  platformType <- case mbPlatformType of
    Nothing -> return APPLICATION
    Just txt -> case readMaybe (unpack txt) of
      Just pt -> return pt
      Nothing -> throwError $ InvalidRequest $ "Invalid platformType: " <> txt

  let vehicleCategoryText = show vehicleType

  integratedBPPConfig <-
    findIntegratedBPPConfig
      (Id <$> mbConfigId)
      merchantOpCityId
      vehicleCategoryText
      platformType

  route <- OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (InvalidRequest $ "Route not found: " <> routeCode)
  routeStops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig

  let serviceableStops = filter (\stop -> stop.timeBounds == Unbounded) routeStops
      stopsSortedBySequenceNumber = sortBy (compare `on` (\s -> s.sequenceNum)) serviceableStops
      firstStop = listToMaybe stopsSortedBySequenceNumber

  stops <-
    if isJust firstStop
      then do
        tripDetails <- OTPRest.getExampleTrip integratedBPPConfig route.id
        case tripDetails of
          Just tripInfo -> do
            let tripStops = tripInfo.stops
                stopSchedules = map (\stop -> Lib.GtfsDataServer.Types.StopSchedule stop.stopCode stop.scheduledArrival stop.scheduledDeparture stop.stopPosition) tripStops
                stopInfos = map (\stop -> Lib.GtfsDataServer.Types.StopInfo stop.stopId stop.stopCode (fromMaybe stop.stopCode stop.stopName) stop.stopPosition stop.lat stop.lon) tripStops
                hashmapSchedule = HashMap.fromList $ map (\stop -> (stop.stopCode, stop)) stopSchedules
                hashmapStop = HashMap.fromList $ map (\stop -> (stop.stopCode, stop)) stopInfos
            foldM
              ( \processedStops stop -> do
                  let stopSchedule = HashMap.lookup stop.stopCode hashmapSchedule
                      stopInfo = HashMap.lookup stop.stopCode hashmapStop
                  let (_, timeTakenToTravelUpcomingStop) =
                        case processedStops of
                          (nextStopSchedule, _) : _ ->
                            case (stopSchedule, nextStopSchedule) of
                              (Just currentSchedule, Just nextSchedule) ->
                                let delta = nextSchedule.arrivalTime - currentSchedule.arrivalTime
                                    adjustedDelta = if delta < 0 then delta + 86400 else delta
                                    validDelta =
                                      if adjustedDelta >= 0 && adjustedDelta <= 14400
                                        then Just adjustedDelta
                                        else Nothing
                                 in (stopSchedule, validDelta)
                              _ -> (stopSchedule, Nothing)
                          [] -> (stopSchedule, Just 0)
                  case stopInfo of
                    Just info ->
                      return $
                        ( stopSchedule,
                          FRFSStationAPI
                            { name = Just info.stopName,
                              code = info.stopCode,
                              routeCodes = Just [route.id],
                              lat = Just info.lat,
                              lon = Just info.lon,
                              timeTakenToTravelUpcomingStop = Seconds <$> timeTakenToTravelUpcomingStop,
                              stationType = Nothing,
                              sequenceNum = Just info.sequenceNum,
                              address = Nothing,
                              distance = Nothing,
                              color = Nothing,
                              towards = Nothing,
                              integratedBppConfigId = stop.integratedBppConfigId,
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
    FRFSRouteAPI
      { code = route.id,
        shortName = fromMaybe "" route.shortName,
        longName = fromMaybe "" route.longName,
        startPoint = route.startPoint,
        endPoint = route.endPoint,
        totalStops = Just $ length stops,
        stops = Just $ map snd stops,
        timeBounds = Nothing,
        waypoints = Nothing,
        integratedBppConfigId = getId integratedBPPConfig.id
      }

-- | Get trip manifest - still proxied to rider-app (needs booking data)
getV2FrfsTripRouteManifest ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    Text ->
    Flow FRFSTripPassengerManifestResp
  )
getV2FrfsTripRouteManifest (_, _merchantId, _merchantOpCityId) tripId routeId = do
  logInfo $ "FRFSFleetOperator: Getting trip manifest for tripId: " <> tripId <> ", routeId: " <> routeId
  bapInternal <- asks (.appBackendBapInternal)
  let riderAppUrl = bapInternal.url
      riderAppApiKey = bapInternal.apiKey
  getFrfsTripManifest riderAppApiKey riderAppUrl tripId routeId

-- | Perform trip action (start, end, reset, rollback)
postFrfsFleetOperatorTripAction ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorTripActionReq ->
    Flow FleetOperatorTripActionResp
  )
postFrfsFleetOperatorTripAction (_, _merchantId, merchantOpCityId) req = do
  let FleetOperatorTripActionReq {action = act} = req
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  let gtfsId = DIBC.feedKey integratedBPPConfig
      anchor =
        GimsOperationAnchor
          { gimsConductorId = req.gimsConductorId,
            gimsDriverId = req.gimsDriverId,
            vehicleNumber = req.vehicleNumber
          }
  gimsOps <- NandiFlow.gimsCurrentOperation baseUrl gtfsId anchor
  let GimsCurrentOperationResp {waybill_no = wbNo, number_of_trips = numTrips, trip_numbers = mbTripNums} = gimsOps
      -- Real (non-dead / non-inactive) trip_numbers in order, e.g. [1,3,4,6,7]. GIMS already
      -- iterated & filtered these; we index into the list so dead trips are skipped. Fall back
      -- to a contiguous range on old GTFS builds that don't send trip_numbers.
      tripNums = fromMaybe [1 .. numTrips] mbTripNums
      configId = getId integratedBPPConfig.id
      redisKey = configId <> ":" <> wbNo <> ":tripnumber"
  now <- getCurrentTime
  let epochNow = round (utcTimeToPOSIXSeconds now * 1000) :: Int64
  logInfo $ "FRFSFleetOperator: Trip action - " <> show act
  case act of
    TripStart -> handleTripStart baseUrl gtfsId anchor tripNums redisKey epochNow wbNo
    TripEnd -> handleTripEnd baseUrl gtfsId anchor redisKey tripNums
    TripReset -> handleTripReset baseUrl gtfsId anchor redisKey tripNums
    TripRollback -> handleTripRollback baseUrl gtfsId anchor redisKey epochNow tripNums
  where
    handleTripStart baseUrl gtfsId anchor tripNums redisKey epochNow wbNo = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip start - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      -- Next real trip_number strictly after the current one (dead trips are absent from tripNums).
      case listToMaybe (filter (> currentTrip) tripNums) of
        Nothing -> do
          void $ Hedis.del lockKey
          throwError $ InvalidRequest "No more trips available for this waybill"
        Just nextTrip -> do
          let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
          flip finally (void $ Hedis.del lockKey) $ do
            void $
              NandiFlow.gimsTripAction
                baseUrl
                gtfsId
                GimsTripActionReq
                  { action = GimsTripActionStart,
                    tripNumber = Just nextTrip,
                    timestamp = Just epochNow,
                    gimsConductorId = ct,
                    gimsDriverId = dt,
                    vehicleNumber = vn
                  }
            Hedis.setExp redisKey nextTrip 172800
            logInfo $ "FRFSFleetOperator: Trip start successful - trip " <> show nextTrip
            -- Notify confirmed passengers (on the rider app) that their bus has started, over the
            -- internal API. Forked so a slow/failed rider-app call never blocks the conductor's start.
            -- tripId matches the rider-app format (`makeTripIdFromWaybillNoAndTripNo`): waybill-tripNo.
            fork "NotifyRiderFrfsTripStarted" $ do
              bapInternal <- asks (.appBackendBapInternal)
              let tripId = wbNo <> "-" <> show nextTrip
              void $ notifyFrfsTripStarted bapInternal.apiKey bapInternal.url tripId
            return $
              FleetOperatorTripActionResp
                { currentTripNumber = nextTrip,
                  hasUpcomingTrips = not (null (filter (> nextTrip) tripNums))
                }

    handleTripEnd baseUrl gtfsId anchor redisKey tripNums = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip end - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      when (currentTrip == 0) $ do
        void $ Hedis.del lockKey
        throwError $ InvalidRequest "No active trip to end"
      let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionEnd,
                tripNumber = Just currentTrip,
                timestamp = Nothing,
                gimsConductorId = ct,
                gimsDriverId = dt,
                vehicleNumber = vn
              }
        logInfo $ "FRFSFleetOperator: Trip end successful - trip " <> show currentTrip
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = currentTrip,
              hasUpcomingTrips = not (null (filter (> currentTrip) tripNums))
            }

    handleTripReset baseUrl gtfsId anchor redisKey tripNums = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip reset - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionReset,
                tripNumber = Nothing,
                timestamp = Nothing,
                gimsConductorId = ct,
                gimsDriverId = dt,
                vehicleNumber = vn
              }
        void $ Hedis.del redisKey
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = 0,
              hasUpcomingTrips = not (null tripNums)
            }

    handleTripRollback baseUrl gtfsId anchor redisKey epochNow tripNums = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip rollback - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      -- Previous real trip_number strictly before the current one (largest tripNum < currentTrip).
      case listToMaybe (reverse (filter (< currentTrip) tripNums)) of
        Nothing -> do
          void $ Hedis.del lockKey
          throwError $ InvalidRequest "No trip to rollback"
        Just rolledBackTrip -> do
          let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
          flip finally (void $ Hedis.del lockKey) $ do
            void $
              NandiFlow.gimsTripAction
                baseUrl
                gtfsId
                GimsTripActionReq
                  { action = GimsTripActionStart,
                    tripNumber = Just rolledBackTrip,
                    timestamp = Just epochNow,
                    gimsConductorId = ct,
                    gimsDriverId = dt,
                    vehicleNumber = vn
                  }
            Hedis.setExp redisKey rolledBackTrip 172800
            logInfo $ "FRFSFleetOperator: Trip rollback successful - trip " <> show rolledBackTrip
            return $
              FleetOperatorTripActionResp
                { currentTripNumber = rolledBackTrip,
                  hasUpcomingTrips = not (null (filter (> rolledBackTrip) tripNums))
                }

-- | Get current operation details
postFrfsFleetOperatorCurrentOperation ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorCurrentOperationReq ->
    Flow FleetOperatorCurrentOperationResp
  )
postFrfsFleetOperatorCurrentOperation (_, _merchantId, merchantOpCityId) req = do
  logInfo "FRFSFleetOperator: Current operation"
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  let gtfsId = DIBC.feedKey integratedBPPConfig
      anchor =
        GimsOperationAnchor
          { gimsConductorId = req.gimsConductorId,
            gimsDriverId = req.gimsDriverId,
            vehicleNumber = req.vehicleNumber
          }
  gimsOps <- NandiFlow.gimsCurrentOperation baseUrl gtfsId anchor
  let configId = getId integratedBPPConfig.id
      redisKey = configId <> ":" <> gimsOps.waybill_no <> ":tripnumber"
  mbPrevTrip <- Hedis.get redisKey
  let prevTrip = fromMaybe 0 (mbPrevTrip :: Maybe Int)
  tripResp <-
    NandiFlow.gimsCurrentTripDetails
      baseUrl
      gtfsId
      GimsCurrentTripDetailsReq
        { previousTripNumber = prevTrip,
          gimsConductorId = req.gimsConductorId,
          gimsDriverId = req.gimsDriverId,
          vehicleNumber = req.vehicleNumber
        }
  let GimsCurrentTripDetailsResp {waybillNo = wNo, vehicleNumber = vNum, gimsConductorId = cToken, gimsDriverId = dToken, history = hist, current = curr, upcoming = upc} = tripResp
  return $
    FleetOperatorCurrentOperationResp
      { waybillNo = wNo,
        vehicleNumber = vNum,
        gtfsId = gtfsId,
        gimsConductorId = cToken,
        gimsDriverId = dToken,
        history = map transformTripInfo hist,
        current = transformTripInfo <$> curr,
        upcoming = map transformTripInfo upc
      }
  where
    transformTripInfo :: GimsTripInfo -> OperatorTripInfo
    transformTripInfo (GimsTripInfo {duty_date = dd, end_time = et, is_active_trip = iat, route_id = rid, route_name = rn, route_number = rnum, start_time = st, trip_number = tn}) =
      OperatorTripInfo
        { dutyDate = dd,
          endTime = et,
          isActiveTrip = iat,
          routeId = rid,
          routeName = rn,
          routeNumber = rnum,
          startTime = st,
          tripNumber = tn
        }
