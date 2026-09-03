module Domain.Action.UI.FRFSFleetOperator
  ( getV2FrfsRoute,
    getV2FrfsTripRouteManifest,
    postFrfsFleetOperatorTripAction,
    postFrfsFleetOperatorTripAction',
    postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorCurrentOperation',
    postFrfsFleetOperatorActiveManifest,
    getV2FrfsBusTripSchedule,
  )
where

import API.Types.UI.FRFSFleetOperator
import BecknV2.FRFS.Enums (VehicleCategory (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (unpack)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Types.FleetOperatorTripAction (FleetOperatorTripAction (..))
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Environment (Flow)
import EulerHS.Prelude hiding (id, unpack)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.MultiModal.Utils (decode)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude (BaseUrl, listToMaybe)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (Meters (..), Minutes (..), Seconds (..))
import Kernel.Types.Id (Id (..), getId)
import Kernel.Types.TimeBound (TimeBound (..))
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common (fork, fromMaybeM, getCurrentTime, highPrecMetersToMeters, logError, logInfo, logWarning, throwError)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import Lib.GtfsDataServer.Types
import SharedLogic.CallBAPInternal (getFrfsTripManifest, notifyFrfsTripStarted)
import SharedLogic.IntegratedBPPConfig (findFirstIbppConfigByCityAndVehicle, findIntegratedBPPConfig, getGimsBaseUrl)
import Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error (GenericError (InvalidRequest))
import Tools.Notifications (NotifReq (..), notifyDriverOnEvents)

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
        waypoints = route.encodedPolyline <&> decode <&> fmap (\point -> LatLong {lat = point.latitude, lon = point.longitude}),
        integratedBppConfigId = getId integratedBPPConfig.id
      }

-- | Get bus trip schedule (per-stop ETAs) directly from GIMS for a given waybill/trip/route.
-- Unlike the manifest above (proxied to rider-app), this hits GIMS' `bus-trip-schedule` endpoint
-- directly via OTPRest, the same way route/stop lookups do.
getV2FrfsBusTripSchedule ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Text ->
    Int ->
    Text ->
    Flow BusTripScheduleResp
  )
getV2FrfsBusTripSchedule (_, _merchantId, merchantOpCityId) routeId tripNumber waybillNo = do
  logInfo $ "FRFSFleetOperator: Getting bus trip schedule for routeId: " <> routeId <> ", waybillNo: " <> waybillNo <> ", tripNumber: " <> show tripNumber
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  schedules <- OTPRest.getBusTripSchedule integratedBPPConfig waybillNo tripNumber routeId
  return $ BusTripScheduleResp {schedules = map mkFleetBusTripSchedule schedules}
  where
    mkFleetBusTripSchedule :: BusScheduleDetail -> FleetBusTripSchedule
    mkFleetBusTripSchedule detail =
      FleetBusTripSchedule
        { eta = map mkFleetBusStopETA detail.eta,
          isActiveTrip = detail.is_active_trip,
          serviceTier = detail.service_tier,
          tripNumber = detail.trip_number,
          vehicleNo = detail.vehicle_no,
          waybillNo = detail.waybill_no
        }
    mkFleetBusStopETA :: BusStopETA -> FleetBusStopETA
    mkFleetBusStopETA e =
      FleetBusStopETA
        { arrivalTime = e.arrivalTime,
          arrivalTimeUnix = fromIntegral e.arrivalTimeUnix,
          etaSeconds = fromIntegral <$> e.etaSeconds,
          stopCode = e.stopCode,
          stopName = e.stopName
        }

frfsCurrentTripRedisKey :: Text -> Text -> Text
frfsCurrentTripRedisKey configId waybillNo = configId <> ":" <> waybillNo <> ":tripnumber"

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

-- | Mirrors rider-app's `makeTripIdFromWaybillNoAndTripNo` -- duplicated here since
-- provider-platform can't import across services.
makeTripIdFromWaybillNoAndTripNo :: Text -> Int -> Text
makeTripIdFromWaybillNoAndTripNo waybillNo tripNo = waybillNo <> "-" <> show tripNo

-- | Perform trip action (start, end, reset, rollback)
postFrfsFleetOperatorTripAction ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorTripActionReq ->
    Flow FleetOperatorTripActionResp
  )
postFrfsFleetOperatorTripAction ctx req = postFrfsFleetOperatorTripAction' ctx False req

-- | Dashboard-aware variant. `isDashboard = True` marks the call as originating from the operator
-- dashboard (already operator-authed by the dashboard layer), which skips the driver-only start/end
-- geofence + lead-time gates below -- a driver can never set this, so it can never bypass the gates.
postFrfsFleetOperatorTripAction' ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool ->
    FleetOperatorTripActionReq ->
    Flow FleetOperatorTripActionResp
  )
postFrfsFleetOperatorTripAction' (_, merchantId, merchantOpCityId) isDashboard req = do
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
      redisKey = frfsCurrentTripRedisKey configId wbNo
  now <- getCurrentTime
  let epochNow = round (utcTimeToPOSIXSeconds now * 1000) :: Int64
  logInfo $ "FRFSFleetOperator: Trip action - " <> show act
  result <- case act of
    -- Only start/end use the per-city geofence/lead-time knobs, so the config is fetched inside those
    -- branches; reset/rollback stay fully independent of any transporter-config read. Non-fatal
    -- (Maybe) so the start/end checks fail open when it's absent.
    TripStart -> do
      mbTransporterConfig <- getTransporterConfig
      handleTripStart integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor tripNums redisKey epochNow wbNo
    TripEnd -> do
      mbTransporterConfig <- getTransporterConfig
      handleTripEnd integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor redisKey epochNow tripNums
    TripReset -> handleTripReset baseUrl gtfsId anchor redisKey tripNums
    TripRollback -> handleTripRollback baseUrl gtfsId anchor redisKey epochNow tripNums
  -- Ops-initiated change the driver's own app has no other way of hearing about; a driver's own
  -- action never sets isDashboard, so this can't notify a driver about their own tap.
  when isDashboard $ notifyDriverOfTripChange merchantId req gimsOps
  pure result
  where
    getTransporterConfig = getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
    handleTripStart integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor tripNums redisKey epochNow wbNo = do
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
            -- Enforcement gates (geofence + 20-min lead time) before committing the start to GIMS.
            -- Fail-open: skipped + logged when config / route / location / schedule is unavailable.
            -- Skipped entirely for dashboard-operator calls (isDashboard), which are already operator-authed.
            withTripRouteChecks isDashboard mbTransporterConfig baseUrl gtfsId req "start" currentTrip nextTrip $ \tc routeId -> do
              validateStartLeadTime integratedBPPConfig wbNo nextTrip routeId (fromMaybe (Minutes 20) tc.tripStartLeadTime)
              mbFirstStop <- boundaryStopPoint integratedBPPConfig routeId True
              validateWithinRadius "start" req.location mbFirstStop (fromMaybe (Meters 500) tc.tripStartGeofenceRadius)
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
            -- Forked so a slow/failed rider-app call never blocks the conductor's start.
            fork "NotifyRiderFrfsTripStarted" $ do
              bapInternal <- asks (.appBackendBapInternal)
              void $ notifyFrfsTripStarted bapInternal.apiKey bapInternal.url (makeTripIdFromWaybillNoAndTripNo wbNo nextTrip)
            return $
              FleetOperatorTripActionResp
                { currentTripNumber = nextTrip,
                  hasUpcomingTrips = not (null (filter (> nextTrip) tripNums))
                }

    handleTripEnd integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor redisKey epochNow tripNums = do
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
        -- Geofence gate (distance to last stop) before committing the end to GIMS. Fail-open:
        -- skipped + logged when config / route / location is unavailable. Skipped entirely for
        -- dashboard-operator calls (isDashboard), which are already operator-authed.
        withTripRouteChecks isDashboard mbTransporterConfig baseUrl gtfsId req "end" currentTrip currentTrip $ \tc routeId -> do
          mbLastStop <- boundaryStopPoint integratedBPPConfig routeId False
          validateWithinRadius "end" req.location mbLastStop (fromMaybe (Meters 1000) tc.tripEndGeofenceRadius)
        void $
          NandiFlow.gimsTripAction
            baseUrl
            gtfsId
            GimsTripActionReq
              { action = GimsTripActionEnd,
                tripNumber = Just currentTrip,
                timestamp = Just epochNow,
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

-- | Best-effort notify: on a dashboard-initiated trip action, ping the driver's phone so their app
-- can refresh immediately instead of waiting for the next poll. Resolves the driver via whichever
-- GIMS token is available -- the request's own, or GIMS's own resolved waybill row (covers
-- vehicle-only dashboard calls that never supplied a driver/conductor token). Fails open: a missing
-- token, an unmatched Person, or a send failure only logs -- it must never break the trip action.
notifyDriverOfTripChange :: Id Domain.Types.Merchant.Merchant -> FleetOperatorTripActionReq -> GimsCurrentOperationResp -> Flow ()
notifyDriverOfTripChange merchantId req gimsOps =
  fork "NotifyDriverFrfsTripChanged" $ do
    let mbToken = req.gimsDriverId <|> req.gimsConductorId <|> gimsOps.gimsDriverId <|> gimsOps.gimsConductorId
    case mbToken of
      Nothing -> logWarning "FRFSFleetOperator: dashboard trip-action notify skipped - no driver/conductor token available"
      Just _ -> do
        mbDriver <- QPerson.findByOperatorBadgeTokenAndMerchantId mbToken merchantId
        case mbDriver of
          Nothing -> logWarning "FRFSFleetOperator: dashboard trip-action notify skipped - no matching driver for token"
          Just driver ->
            notifyDriverOnEvents
              driver.merchantOperatingCityId
              driver.id
              driver.deviceToken
              NotifReq {entityId = driver.id.getId, title = "Trip updated", message = "Your trip was updated. Tap to refresh."}
              FCM.TRIP_UPDATED

-- | Get current operation details
postFrfsFleetOperatorCurrentOperation ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorCurrentOperationReq ->
    Flow FleetOperatorCurrentOperationResp
  )
postFrfsFleetOperatorCurrentOperation ctx req = postFrfsFleetOperatorCurrentOperation' ctx False req

-- | Dashboard-aware variant. `isDashboard = True` marks the call as originating
-- from the operator dashboard so future driver-only gates can be overridden.
postFrfsFleetOperatorCurrentOperation' ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool ->
    FleetOperatorCurrentOperationReq ->
    Flow FleetOperatorCurrentOperationResp
  )
postFrfsFleetOperatorCurrentOperation' (_, _merchantId, merchantOpCityId) _isDashboard req = do
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
      redisKey = frfsCurrentTripRedisKey configId gimsOps.waybill_no
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

-- | Resolve the vehicle's currently active trip from an anchor and return its manifest in the same
-- call -- no client-supplied tripId needed to know what to poll. Tries GIMS's activeTrip endpoint
-- first (one lean call, no previousTripNumber, no history/upcoming bucketing); on failure, falls
-- back to the client's own last-known tripId/routeId so a GIMS blip degrades to stale-but-working
-- rather than losing the passenger list.
postFrfsFleetOperatorActiveManifest ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FRFSActiveManifestReq ->
    Flow FRFSActiveManifestResp
  )
postFrfsFleetOperatorActiveManifest (_, _merchantId, merchantOpCityId) req = do
  logInfo "FRFSFleetOperator: Active manifest"
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
  mbActiveTrip <- NandiFlow.gimsActiveTrip baseUrl gtfsId anchor
  let (mbTripId, mbRouteId) = case mbActiveTrip of
        Just activeTrip ->
          ( makeTripIdFromWaybillNoAndTripNo activeTrip.waybill_no <$> activeTrip.active_trip_number,
            activeTrip.route_id
          )
        Nothing -> (req.tripId, req.routeId)
  mbManifest <- case (mbTripId, mbRouteId) of
    (Just tripId, Just routeId) -> do
      bapInternal <- asks (.appBackendBapInternal)
      Just <$> getFrfsTripManifest bapInternal.apiKey bapInternal.url tripId routeId
    _ -> pure Nothing
  pure
    FRFSActiveManifestResp
      { tripId = mbTripId,
        routeId = mbRouteId,
        manifest = maybe [] (.manifest) mbManifest
      }

-- | Shared scaffold for the start/end enforcement gates: when the config is present and the trip's
-- route resolves, hand (config, routeId) to @runChecks@; otherwise fail open with a skip-log at the
-- first missing step. Polymorphic in the config so it needs no TransporterConfig import; the concrete
-- type is pinned by the @mbConfig@ argument at each call site.
withTripRouteChecks ::
  Bool ->
  Maybe cfg ->
  BaseUrl ->
  Text ->
  FleetOperatorTripActionReq ->
  Text ->
  Int ->
  Int ->
  (cfg -> Text -> Flow ()) ->
  Flow ()
withTripRouteChecks bypassChecks mbConfig baseUrl gtfsId req label previousTripNumber targetTripNumber runChecks
  | bypassChecks = logInfo $ "FRFSFleetOperator: trip " <> label <> " checks bypassed - dashboard operator action"
  | otherwise =
    case mbConfig of
      Nothing -> logWarning $ "FRFSFleetOperator: trip " <> label <> " checks skipped - TransporterConfig not found"
      Just cfg -> do
        mbRouteId <- resolveTripRouteId baseUrl gtfsId req previousTripNumber targetTripNumber
        case mbRouteId of
          Nothing -> logWarning $ "FRFSFleetOperator: trip " <> label <> " checks skipped - could not resolve route for trip"
          Just routeId -> runChecks cfg routeId

-- | Resolve the route_id of a specific trip via currentTripDetails (which carries per-trip route_id,
-- unlike the cheaper currentOperation). Returns Nothing (caller fails open) when it can't be found.
resolveTripRouteId :: BaseUrl -> Text -> FleetOperatorTripActionReq -> Int -> Int -> Flow (Maybe Text)
resolveTripRouteId baseUrl gtfsId req previousTripNumber targetTripNumber = do
  resp <-
    NandiFlow.gimsCurrentTripDetails baseUrl gtfsId $
      GimsCurrentTripDetailsReq
        { previousTripNumber = previousTripNumber,
          gimsConductorId = req.gimsConductorId,
          gimsDriverId = req.gimsDriverId,
          vehicleNumber = req.vehicleNumber
        }
  let allTrips = resp.upcoming <> maybe [] (: []) resp.current <> resp.history
  pure $ (.route_id) <$> find (\t -> t.trip_number == targetTripNumber) allTrips

-- | First (or last) stop point of a route, by stop sequence.
boundaryStopPoint :: DIBC.IntegratedBPPConfig -> Text -> Bool -> Flow (Maybe LatLong)
boundaryStopPoint integratedBPPConfig routeCode wantFirstStop = do
  stops <- OTPRest.getRouteStopMappingByRouteCode routeCode integratedBPPConfig
  let sorted = sortOn (.sequenceNum) stops
  pure $ (.stopPoint) <$> (if wantFirstStop then listToMaybe sorted else listToMaybe (reverse sorted))

-- | Geofence: throw when the driver is beyond @radius@ of the boundary stop. Fail-open (log) when
-- the driver location or the resolved stop point is unavailable.
validateWithinRadius :: Text -> Maybe LatLong -> Maybe LatLong -> Meters -> Flow ()
validateWithinRadius boundaryLabel mbLocation mbStopPoint radius =
  case (mbLocation, mbStopPoint) of
    (Just location, Just stopPoint) -> do
      let distance = highPrecMetersToMeters (distanceBetweenInMeters location stopPoint)
      when (distance > radius) $
        throwError $
          InvalidRequest $
            "You are too far from the trip " <> boundaryLabel <> " stop (" <> show distance.getMeters <> "m away, allowed within " <> show radius.getMeters <> "m)."
    (Nothing, _) -> logWarning $ "FRFSFleetOperator: trip " <> boundaryLabel <> " geofence skipped - no driver location supplied"
    (_, Nothing) -> logWarning $ "FRFSFleetOperator: trip " <> boundaryLabel <> " geofence skipped - could not resolve " <> boundaryLabel <> " stop"

-- | Lead-time: allow a start only within @leadTime@ before the scheduled start. Scheduled start is
-- the first stop's ETA epoch from the bus schedule (an absolute instant, so no timezone parsing; we
-- read the raw unix field, not the pre-IST-shifted arrivalTime). Fail-open when schedule is missing.
validateStartLeadTime :: DIBC.IntegratedBPPConfig -> Text -> Int -> Text -> Minutes -> Flow ()
validateStartLeadTime integratedBPPConfig waybillNo tripNumber routeId leadTime = do
  schedules <- OTPRest.getBusTripSchedule integratedBPPConfig waybillNo tripNumber routeId
  let mbScheduledStartEpoch = do
        detail <- listToMaybe schedules
        firstStop <- listToMaybe detail.eta
        pure firstStop.arrivalTimeUnix
  case mbScheduledStartEpoch of
    Nothing -> logWarning "FRFSFleetOperator: trip start lead-time check skipped - schedule unavailable"
    Just epochSecs -> do
      now <- getCurrentTime
      let scheduledStart = posixSecondsToUTCTime (fromIntegral epochSecs)
          leadWindow = fromIntegral (leadTime.getMinutes * 60) :: NominalDiffTime
      when (diffUTCTime scheduledStart now > leadWindow) $
        throwError $
          InvalidRequest $
            "This trip can only be started within " <> show leadTime.getMinutes <> " minutes of its scheduled start time."
