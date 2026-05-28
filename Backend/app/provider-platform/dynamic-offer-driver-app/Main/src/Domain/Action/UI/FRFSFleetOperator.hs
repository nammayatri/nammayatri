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
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id (Id (..), getId)
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, logError, logInfo, throwError)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.CallBAPInternal (getFrfsTripManifest)
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig, getBaseUrl)
import Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QPerson
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
getV2FrfsTripRouteManifest (_, _merchantId, merchantOpCityId) tripId routeId = do
  logInfo $ "FRFSFleetOperator: Getting trip manifest for tripId: " <> tripId <> ", routeId: " <> routeId
  bapInternal <- asks (.appBackendBapInternal)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (InvalidRequest $ "MerchantOperatingCity not found: " <> merchantOpCityId.getId)
  let riderAppUrl = bapInternal.url
      riderAppApiKey = bapInternal.apiKey
  getFrfsTripManifest riderAppApiKey riderAppUrl tripId routeId merchantOpCity.city

-- | Perform trip action (start, end, reset, rollback)
postFrfsFleetOperatorTripAction ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorTripActionReq ->
    Flow FleetOperatorTripActionResp
  )
postFrfsFleetOperatorTripAction (mbCallerId, _merchantId, merchantOpCityId) req = do
  let FleetOperatorTripActionReq {action = act, personId = pid} = req
  callerId <- mbCallerId & fromMaybeM (InvalidRequest "Unauthorized")
  unless (getId callerId == pid) $
    throwError $ InvalidRequest "Unauthorized: personId mismatch"

  person <- QPerson.findById callerId >>= fromMaybeM (PersonNotFound pid)
  condToken <- person.operatorBadgeToken & fromMaybeM (InvalidRequest $ "No operatorBadgeToken for person: " <> pid)

  integratedBPPConfig <-
    findIntegratedBPPConfig
      Nothing
      merchantOpCityId
      (show BUS)
      MULTIMODAL

  baseUrl <- getBaseUrl integratedBPPConfig

  let anchor =
        GimsOperationAnchor
          { conductor_token = Just condToken,
            driver_token = Nothing,
            vehicle_number = Nothing
          }

  gimsOps <- NandiFlow.gimsCurrentOperation baseUrl (DIBC.feedKey integratedBPPConfig) anchor
  let GimsCurrentOperationResp {waybill_no = wbNo, number_of_trips = numTrips} = gimsOps

  let configId = getId integratedBPPConfig.id
      redisKey = configId <> ":" <> condToken <> ":" <> wbNo <> ":tripnumber"
  now <- getCurrentTime
  let epochNow = round (utcTimeToPOSIXSeconds now * 1000) :: Int64

  logInfo $ "FRFSFleetOperator: Trip action - " <> show act
  result <- case act of
    TripStart -> handleTripStart baseUrl (DIBC.feedKey integratedBPPConfig) anchor numTrips redisKey epochNow
    TripEnd -> handleTripEnd baseUrl (DIBC.feedKey integratedBPPConfig) anchor redisKey numTrips
    TripReset -> handleTripReset baseUrl (DIBC.feedKey integratedBPPConfig) anchor redisKey numTrips
    TripRollback -> handleTripRollback baseUrl (DIBC.feedKey integratedBPPConfig) anchor redisKey epochNow numTrips

  return result
  where
    handleTripStart baseUrl gtfsId anchor numTrips redisKey epochNow = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip start - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      when (currentTrip >= numTrips) $ do
        void $ Hedis.del lockKey
        throwError $ InvalidRequest "No more trips available for this waybill"
      let nextTrip = currentTrip + 1
          GimsOperationAnchor {conductor_token = ct, driver_token = dt, vehicle_number = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionStart,
                trip_number = Just nextTrip,
                timestamp = Just epochNow,
                conductor_token = ct,
                driver_token = dt,
                vehicle_number = vn
              }
        Hedis.setExp redisKey nextTrip 172800
        logInfo $ "FRFSFleetOperator: Trip start successful - trip " <> show nextTrip
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = nextTrip,
              hasUpcomingTrips = nextTrip < numTrips
            }

    handleTripEnd baseUrl gtfsId anchor redisKey numTrips = do
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
      let GimsOperationAnchor {conductor_token = ct, driver_token = dt, vehicle_number = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionEnd,
                trip_number = Just currentTrip,
                timestamp = Nothing,
                conductor_token = ct,
                driver_token = dt,
                vehicle_number = vn
              }
        logInfo $ "FRFSFleetOperator: Trip end successful - trip " <> show currentTrip
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = currentTrip,
              hasUpcomingTrips = currentTrip < numTrips
            }

    handleTripReset baseUrl gtfsId anchor redisKey numTrips = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip reset - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      let GimsOperationAnchor {conductor_token = ct, driver_token = dt, vehicle_number = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionReset,
                trip_number = Nothing,
                timestamp = Nothing,
                conductor_token = ct,
                driver_token = dt,
                vehicle_number = vn
              }
        void $ Hedis.del redisKey
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = 0,
              hasUpcomingTrips = numTrips > 0
            }

    handleTripRollback baseUrl gtfsId anchor redisKey epochNow numTrips = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip rollback - " <> redisKey
        throwError $ InvalidRequest "Could not acquire lock for trip action"
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      when (currentTrip <= 1) $ do
        void $ Hedis.del lockKey
        throwError $ InvalidRequest "No trip to rollback"
      let rolledBackTrip = currentTrip - 1
          GimsOperationAnchor {conductor_token = ct, driver_token = dt, vehicle_number = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionStart,
                trip_number = Just rolledBackTrip,
                timestamp = Just epochNow,
                conductor_token = ct,
                driver_token = dt,
                vehicle_number = vn
              }
        Hedis.setExp redisKey rolledBackTrip 172800
        logInfo $ "FRFSFleetOperator: Trip rollback successful - trip " <> show rolledBackTrip
        return $
          FleetOperatorTripActionResp
            { currentTripNumber = rolledBackTrip,
              hasUpcomingTrips = rolledBackTrip < numTrips
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
postFrfsFleetOperatorCurrentOperation (mbCallerId, _merchantId, merchantOpCityId) req = do
  let FleetOperatorCurrentOperationReq {personId = pid} = req
  logInfo "FRFSFleetOperator: Current operation"
  callerId <- mbCallerId & fromMaybeM (InvalidRequest "Unauthorized")
  unless (getId callerId == pid) $
    throwError $ InvalidRequest "Unauthorized: personId mismatch"

  person <- QPerson.findById callerId >>= fromMaybeM (PersonNotFound pid)
  condToken <- person.operatorBadgeToken & fromMaybeM (InvalidRequest $ "No operatorBadgeToken for person: " <> pid)

  -- 1.Find BPP config for BUS/MULTIMODAL
  integratedBPPConfig <-
    findIntegratedBPPConfig
      Nothing
      merchantOpCityId
      (show BUS)
      MULTIMODAL

  -- 2.Get OTP REST base URL
  baseUrl <- getBaseUrl integratedBPPConfig

  let anchor =
        GimsOperationAnchor
          { conductor_token = Just condToken,
            driver_token = Nothing,
            vehicle_number = Nothing
          }

  -- 3.Get current operation (waybill)
  gimsOps <- NandiFlow.gimsCurrentOperation baseUrl (DIBC.feedKey integratedBPPConfig) anchor
  let GimsCurrentOperationResp {waybill_no = wbNo} = gimsOps

  -- 4.Get trip number from Redis
  let configId = getId integratedBPPConfig.id
      redisKey = configId <> ":" <> condToken <> ":" <> wbNo <> ":tripnumber"
  mbPrevTrip <- Hedis.get redisKey
  let prevTrip = fromMaybe 0 (mbPrevTrip :: Maybe Int)

  -- 5.Get trip details
  tripResp <-
    NandiFlow.gimsCurrentTripDetails
      baseUrl
      (DIBC.feedKey integratedBPPConfig)
      GimsCurrentTripDetailsReq
        { previous_trip_number = prevTrip,
          conductor_token = Just condToken,
          driver_token = Nothing,
          vehicle_number = Nothing
        }

  -- 6.Transform to API response
  let GimsCurrentTripDetailsResp {waybill_no = wNo, vehicle_number = vNum, conductor_token = cToken, driver_token = dToken, history = hist, current = curr, upcoming = upc} = tripResp
  return $
    FleetOperatorCurrentOperationResp
      { waybillNo = wNo,
        vehicleNumber = vNum,
        gtfsId = DIBC.feedKey integratedBPPConfig,
        conductorToken = Just cToken,
        driverToken = Just dToken,
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
