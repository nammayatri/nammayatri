module Domain.Action.UI.FRFSFleetOperator
  ( getV2FrfsRoute,
    getV2FrfsTripRouteManifest,
    postFrfsFleetOperatorTripAction,
    postFrfsFleetOperatorTripAction',
    postFrfsFleetOperatorCurrentOperation,
    postFrfsFleetOperatorCurrentOperation',
    postFrfsFleetOperatorActiveManifest,
    getV2FrfsBusTripSchedule,
    postFrfsFleetOperatorV2TripAction,
    postFrfsFleetOperatorV2TripAction',
    postFrfsFleetOperatorV2CurrentOperation,
    postFrfsFleetOperatorV2CurrentOperation',
    postFrfsFleetOperatorV2ActiveManifest,
  )
where

import API.Types.UI.FRFSFleetOperator
import BecknV2.FRFS.Enums (VehicleCategory (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Text (unpack)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Types.FleetOperatorTripAction (FleetOperatorTripAction (..))
import Domain.Types.FleetOperatorTripActionV2 (FleetOperatorTripActionV2 (..), toGimsV2TripAction)
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
import Tools.Error (FRFSFleetOperatorTripActionError (..), GenericError (InvalidRequest))
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

-- | Prefer the authenticated caller's own GIMS identity over whatever the request claims, so a
-- driver-initiated call can't act on GIMS as a different driver/conductor than the one it actually
-- authenticated as. Only kicks in when there's an authenticated Person with a GIMS badge token and a
-- bus driver/conductor role on file; dashboard calls (ops, not a driver) and anything else fall back
-- to whatever anchor the request itself supplied.
resolveEmployeeGimsAnchor :: Maybe (Id Domain.Types.Person.Person) -> Flow (Maybe GimsOperationAnchor)
resolveEmployeeGimsAnchor Nothing = pure Nothing
resolveEmployeeGimsAnchor (Just personId) = do
  mbPerson <- QPerson.findById personId
  pure $
    mbPerson >>= \person -> do
      token <- person.operatorBadgeToken
      case person.role of
        Domain.Types.Person.BUS_CONDUCTOR -> Just GimsOperationAnchor {gimsConductorId = Just token, gimsDriverId = Nothing, vehicleNumber = Nothing}
        Domain.Types.Person.BUS_DRIVER -> Just GimsOperationAnchor {gimsConductorId = Nothing, gimsDriverId = Just token, vehicleNumber = Nothing}
        _ -> Nothing

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
postFrfsFleetOperatorTripAction' (mbPersonId, merchantId, merchantOpCityId) isDashboard req = do
  let FleetOperatorTripActionReq {action = act} = req
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  mbDerivedAnchor <- if isDashboard then pure Nothing else resolveEmployeeGimsAnchor mbPersonId
  let gtfsId = DIBC.feedKey integratedBPPConfig
      anchor =
        fromMaybe
          GimsOperationAnchor
            { gimsConductorId = req.gimsConductorId,
              gimsDriverId = req.gimsDriverId,
              vehicleNumber = req.vehicleNumber
            }
          mbDerivedAnchor
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
      handleTripEnd integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor redisKey epochNow tripNums wbNo
    TripReset -> handleTripReset baseUrl gtfsId anchor redisKey tripNums wbNo
    TripRollback -> handleTripRollback baseUrl gtfsId anchor redisKey epochNow tripNums wbNo
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
        throwError $ TripActionLockNotAcquired "start" wbNo req.vehicleNumber
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      -- Next real trip_number strictly after the current one (dead trips are absent from tripNums).
      case listToMaybe (filter (> currentTrip) tripNums) of
        Nothing -> do
          void $ Hedis.del lockKey
          throwError $ NoMoreTripsAvailable wbNo req.vehicleNumber
        Just nextTrip -> do
          let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
          flip finally (void $ Hedis.del lockKey) $ do
            -- Enforcement gates (geofence + 20-min lead time) before committing the start to GIMS.
            -- Fail-open: skipped + logged when config / route / location / schedule is unavailable.
            -- Skipped entirely for dashboard-operator calls (isDashboard), which are already operator-authed.
            withTripRouteChecks isDashboard mbTransporterConfig baseUrl gtfsId anchor "start" currentTrip nextTrip $ \tc routeId -> do
              validateStartLeadTime integratedBPPConfig wbNo req.vehicleNumber nextTrip routeId (fromMaybe (Minutes 20) tc.tripStartLeadTime)
              mbFirstStop <- boundaryStopPoint integratedBPPConfig routeId True
              validateWithinRadius "start" wbNo req.vehicleNumber req.location mbFirstStop (fromMaybe (Meters 500) tc.tripStartGeofenceRadius)
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

    handleTripEnd integratedBPPConfig mbTransporterConfig baseUrl gtfsId anchor redisKey epochNow tripNums wbNo = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip end - " <> redisKey
        throwError $ TripActionLockNotAcquired "end" wbNo req.vehicleNumber
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      when (currentTrip == 0) $ do
        void $ Hedis.del lockKey
        throwError $ NoActiveTripToEnd wbNo req.vehicleNumber
      let GimsOperationAnchor {gimsConductorId = ct, gimsDriverId = dt, vehicleNumber = vn} = anchor
      flip finally (void $ Hedis.del lockKey) $ do
        -- Geofence gate (distance to last stop) before committing the end to GIMS. Fail-open:
        -- skipped + logged when config / route / location is unavailable. Skipped entirely for
        -- dashboard-operator calls (isDashboard), which are already operator-authed.
        withTripRouteChecks isDashboard mbTransporterConfig baseUrl gtfsId anchor "end" currentTrip currentTrip $ \tc routeId -> do
          mbLastStop <- boundaryStopPoint integratedBPPConfig routeId False
          validateWithinRadius "end" wbNo req.vehicleNumber req.location mbLastStop (fromMaybe (Meters 1000) tc.tripEndGeofenceRadius)
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

    handleTripReset baseUrl gtfsId anchor redisKey tripNums wbNo = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip reset - " <> redisKey
        throwError $ TripActionLockNotAcquired "reset" wbNo req.vehicleNumber
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

    handleTripRollback baseUrl gtfsId anchor redisKey epochNow tripNums wbNo = do
      let lockKey = redisKey <> ":lock"
      lockAcquired <- Hedis.setNxExpire lockKey 30 ("1" :: Text)
      unless lockAcquired $ do
        logError $ "FRFSFleetOperator: Could not acquire lock for trip rollback - " <> redisKey
        throwError $ TripActionLockNotAcquired "rollback" wbNo req.vehicleNumber
      mbCurrentTrip <- Hedis.get redisKey
      let currentTrip = fromMaybe 0 (mbCurrentTrip :: Maybe Int)
      -- Previous real trip_number strictly before the current one (largest tripNum < currentTrip).
      case listToMaybe (reverse (filter (< currentTrip) tripNums)) of
        Nothing -> do
          void $ Hedis.del lockKey
          throwError $ NoTripToRollback wbNo req.vehicleNumber
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

-- | Best-effort notify: on a dashboard-initiated trip action, ping both the driver's and the
-- conductor's phones -- a bus can have both assigned at once, so this can't just pick one -- so
-- their apps can refresh immediately instead of waiting for the next poll. Resolves each via
-- whichever GIMS token is available -- the request's own, or GIMS's own resolved waybill row
-- (covers vehicle-only dashboard calls that never supplied a driver/conductor token). Fails open
-- per person: a missing token, an unmatched Person, or a send failure only logs -- it must never
-- break the trip action.
notifyDriverOfTripChange :: Id Domain.Types.Merchant.Merchant -> FleetOperatorTripActionReq -> GimsCurrentOperationResp -> Flow ()
notifyDriverOfTripChange merchantId req gimsOps =
  fork "NotifyDriverFrfsTripChanged" $ do
    notifyByToken (req.gimsDriverId <|> gimsOps.gimsDriverId)
    notifyByToken (req.gimsConductorId <|> gimsOps.gimsConductorId)
  where
    notifyByToken Nothing = pure ()
    notifyByToken mbToken = do
      mbDriver <- QPerson.findByOperatorBadgeTokenAndMerchantId mbToken merchantId
      case mbDriver of
        Nothing -> logWarning "FRFSFleetOperator: dashboard trip-action notify skipped - no matching driver/conductor for token"
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
postFrfsFleetOperatorCurrentOperation' (mbPersonId, _merchantId, merchantOpCityId) isDashboard req = do
  logInfo "FRFSFleetOperator: Current operation"
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  mbDerivedAnchor <- if isDashboard then pure Nothing else resolveEmployeeGimsAnchor mbPersonId
  let gtfsId = DIBC.feedKey integratedBPPConfig
      anchor =
        fromMaybe
          GimsOperationAnchor
            { gimsConductorId = req.gimsConductorId,
              gimsDriverId = req.gimsDriverId,
              vehicleNumber = req.vehicleNumber
            }
          mbDerivedAnchor
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
          gimsConductorId = anchor.gimsConductorId,
          gimsDriverId = anchor.gimsDriverId,
          vehicleNumber = anchor.vehicleNumber
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

-- | Resolve the caller's own currently active trip and return its manifest in the same call -- no
-- client-supplied tripId needed to know what to poll, and no client-supplied GIMS identity either:
-- the driver calling this is always resolved server-side from the authenticated session, via
-- `resolveEmployeeGimsAnchor`. Falls back to the client's own last-known tripId/routeId whenever that
-- resolution can't happen (no matching Person/badge token, or GIMS itself is down), so a GIMS blip
-- degrades to stale-but-working rather than losing the passenger list.
postFrfsFleetOperatorActiveManifest ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FRFSActiveManifestReq ->
    Flow FRFSActiveManifestResp
  )
postFrfsFleetOperatorActiveManifest (mbPersonId, _merchantId, merchantOpCityId) req = do
  logInfo "FRFSFleetOperator: Active manifest"
  integratedBPPConfig <-
    findFirstIbppConfigByCityAndVehicle
      merchantOpCityId
      (show BUS)
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  mbAnchor <- resolveEmployeeGimsAnchor mbPersonId
  let gtfsId = DIBC.feedKey integratedBPPConfig
  mbActiveTrip <- case mbAnchor of
    Just anchor -> NandiFlow.gimsActiveTrip baseUrl gtfsId anchor
    Nothing -> pure Nothing
  let (mbTripId, mbRouteId) = case mbActiveTrip of
        Just activeTrip ->
          ( makeTripIdFromWaybillNoAndTripNo activeTrip.waybill_no <$> activeTrip.active_trip_number,
            activeTrip.route_id
          )
        Nothing -> (req.tripId, req.routeId)
  mbManifest <- case (mbTripId, mbRouteId) of
    (Just tripId, Just routeId) -> Just <$> getV2FrfsTripRouteManifest (mbPersonId, _merchantId, merchantOpCityId) tripId routeId
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
  GimsOperationAnchor ->
  Text ->
  Int ->
  Int ->
  (cfg -> Text -> Flow ()) ->
  Flow ()
withTripRouteChecks bypassChecks mbConfig baseUrl gtfsId anchor label previousTripNumber targetTripNumber runChecks
  | bypassChecks = logInfo $ "FRFSFleetOperator: trip " <> label <> " checks bypassed - dashboard operator action"
  | otherwise =
    case mbConfig of
      Nothing -> logWarning $ "FRFSFleetOperator: trip " <> label <> " checks skipped - TransporterConfig not found"
      Just cfg -> do
        mbRouteId <- resolveTripRouteId baseUrl gtfsId anchor previousTripNumber targetTripNumber
        case mbRouteId of
          Nothing -> logWarning $ "FRFSFleetOperator: trip " <> label <> " checks skipped - could not resolve route for trip"
          Just routeId -> runChecks cfg routeId

-- | Resolve the route_id of a specific trip via currentTripDetails (which carries per-trip route_id,
-- unlike the cheaper currentOperation). Returns Nothing (caller fails open) when it can't be found.
resolveTripRouteId :: BaseUrl -> Text -> GimsOperationAnchor -> Int -> Int -> Flow (Maybe Text)
resolveTripRouteId baseUrl gtfsId anchor previousTripNumber targetTripNumber = do
  resp <-
    NandiFlow.gimsCurrentTripDetails baseUrl gtfsId $
      GimsCurrentTripDetailsReq
        { previousTripNumber = previousTripNumber,
          gimsConductorId = anchor.gimsConductorId,
          gimsDriverId = anchor.gimsDriverId,
          vehicleNumber = anchor.vehicleNumber
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
validateWithinRadius :: Text -> Text -> Maybe Text -> Maybe LatLong -> Maybe LatLong -> Meters -> Flow ()
validateWithinRadius boundaryLabel waybillNo vehicleNumber mbLocation mbStopPoint radius =
  case (mbLocation, mbStopPoint) of
    (Just location, Just stopPoint) -> do
      let distance = highPrecMetersToMeters (distanceBetweenInMeters location stopPoint)
      when (distance > radius) $
        throwError $
          TripGeofenceViolation
            ("You are too far from the trip " <> boundaryLabel <> " stop (" <> show distance.getMeters <> "m away, allowed within " <> show radius.getMeters <> "m).")
            waybillNo
            vehicleNumber
    (Nothing, _) -> logWarning $ "FRFSFleetOperator: trip " <> boundaryLabel <> " geofence skipped - no driver location supplied"
    (_, Nothing) -> logWarning $ "FRFSFleetOperator: trip " <> boundaryLabel <> " geofence skipped - could not resolve " <> boundaryLabel <> " stop"

-- | Lead-time: allow a start only within @leadTime@ before the scheduled start. Scheduled start is
-- the first stop's ETA epoch from the bus schedule (an absolute instant, so no timezone parsing; we
-- read the raw unix field, not the pre-IST-shifted arrivalTime). Fail-open when schedule is missing.
validateStartLeadTime :: DIBC.IntegratedBPPConfig -> Text -> Maybe Text -> Int -> Text -> Minutes -> Flow ()
validateStartLeadTime integratedBPPConfig waybillNo vehicleNumber tripNumber routeId leadTime = do
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
          TripStartTooEarly
            ("This trip can only be started within " <> show leadTime.getMinutes <> " minutes of its scheduled start time.")
            waybillNo
            vehicleNumber

-- ─── transitV2 (GIMS runs / duties) ──────────────────────────────────────────
-- GIMS owns trip state (no Redis cursor) and enforces order. This layer keeps what only the
-- driver app can do: resolve the caller's own GIMS identity, the start/end geofence and
-- lead-time gates for driver calls, rider notifications on start, and the driver push on
-- dashboard changes. Plan: scripts/plans/gims/transitV2.

data TransitV2Ctx = TransitV2Ctx
  { ibppConfig :: DIBC.IntegratedBPPConfig,
    gimsUrl :: BaseUrl,
    gtfsId :: Text
  }

transitV2Ctx :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Flow TransitV2Ctx
transitV2Ctx merchantOpCityId = do
  ibppConfig <- findFirstIbppConfigByCityAndVehicle merchantOpCityId (show BUS)
  gimsUrl <- getGimsBaseUrl ibppConfig
  pure TransitV2Ctx {ibppConfig, gimsUrl, gtfsId = DIBC.feedKey ibppConfig}

-- | Driver calls act as the authenticated driver / conductor (never a client-chosen run);
-- dashboard calls use the request's anchor, including a run id.
transitV2Anchor :: Bool -> Maybe (Id Domain.Types.Person.Person) -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow GimsV2Anchor
transitV2Anchor isDashboard mbPersonId conductorToken driverToken vehicleNumber dutyGroupId
  | isDashboard = pure GimsV2Anchor {vehicleNumber, driverToken, conductorToken, dutyGroupId}
  | otherwise = do
    mbDerived <- resolveEmployeeGimsAnchor mbPersonId
    pure $ case mbDerived of
      Just a -> GimsV2Anchor {vehicleNumber = Nothing, driverToken = a.gimsDriverId, conductorToken = a.gimsConductorId, dutyGroupId = Nothing}
      Nothing -> GimsV2Anchor {vehicleNumber, driverToken, conductorToken, dutyGroupId = Nothing}

postFrfsFleetOperatorV2TripAction ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorTripActionV2Req ->
    Flow FleetOperatorCurrentOperationV2Resp
  )
postFrfsFleetOperatorV2TripAction ctx req = postFrfsFleetOperatorV2TripAction' ctx False Nothing Nothing req

-- | `isDashboard = True`: ops call (skips the driver gates, may cancel / uncancel, pushes the
-- driver). `mbOperatorId` / `mbActorId` go to GIMS as x-operator-id / x-actor-person-id.
postFrfsFleetOperatorV2TripAction' ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool ->
    Maybe Text ->
    Maybe Text ->
    FleetOperatorTripActionV2Req ->
    Flow FleetOperatorCurrentOperationV2Resp
  )
postFrfsFleetOperatorV2TripAction' (mbPersonId, merchantId, merchantOpCityId) isDashboard mbOperatorId mbActorId req = do
  when (not isDashboard && req.action `elem` [TripV2Cancel, TripV2Uncancel]) $
    throwError $ InvalidRequest "Trips can only be cancelled from the dashboard"
  ctx <- transitV2Ctx merchantOpCityId
  anchor <- transitV2Anchor isDashboard mbPersonId req.gimsConductorId req.gimsDriverId req.vehicleNumber req.dutyGroupId
  let actorId = mbActorId <|> (getId <$> mbPersonId)
  unless isDashboard $ transitV2DriverGates ctx merchantOpCityId anchor req
  now <- getCurrentTime
  let epochNow = round (utcTimeToPOSIXSeconds now * 1000) :: Int64
  logInfo $ "FRFSFleetOperator v2: trip action " <> show req.action
  resp <-
    NandiFlow.gimsV2TripAction
      ctx.gimsUrl
      ctx.gtfsId
      mbOperatorId
      actorId
      GimsV2TripActionReq
        { vehicleNumber = anchor.vehicleNumber,
          driverToken = anchor.driverToken,
          conductorToken = anchor.conductorToken,
          dutyGroupId = anchor.dutyGroupId,
          action = toGimsV2TripAction req.action,
          tripNumber = req.tripNumber,
          timestamp = Just epochNow,
          -- a skip from the driver's own app is recorded as DRIVER unless a reason was sent
          reason = req.reason <|> (if not isDashboard && req.action == TripV2Skip then Just "DRIVER" else Nothing)
        }
  -- `resp` is the run after the action; on start its running trip is the one just started.
  when (req.action == TripV2Start) $
    whenJust ((.tripNumber) <$> resp.active) $ \tripNo ->
      -- Forked so a slow/failed rider-app call never blocks the start.
      fork "NotifyRiderFrfsTripStartedV2" $ do
        bapInternal <- asks (.appBackendBapInternal)
        void $ notifyFrfsTripStarted bapInternal.apiKey bapInternal.url (makeTripIdFromWaybillNoAndTripNo resp.waybillNo tripNo)
  when isDashboard $
    fork "NotifyDriverFrfsTripChangedV2" $ do
      let tokens = catMaybes [resp.driverToken, resp.conductorToken] <> concatMap (\t -> catMaybes [t.driverTokenNumber, t.conductorTokenNumber]) (maybeToList resp.active <> take 1 resp.upcoming)
      mapM_ (notifyTripChangedByToken merchantId) (ordNub tokens)
  toCurrentOperationV2 ctx resp

-- | Start: lead time + distance to the first stop of the trip about to start. End: distance to
-- the last stop of the running trip. Fail open (log) when config / trip / location is missing,
-- like the v1 gates.
transitV2DriverGates :: TransitV2Ctx -> Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> GimsV2Anchor -> FleetOperatorTripActionV2Req -> Flow ()
transitV2DriverGates ctx merchantOpCityId anchor req =
  when (req.action `elem` [TripV2Start, TripV2End]) $ do
    mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
    case mbTransporterConfig of
      Nothing -> logWarning "FRFSFleetOperator v2: trip checks skipped - TransporterConfig not found"
      Just tc -> do
        op <- NandiFlow.gimsV2CurrentOperation ctx.gimsUrl ctx.gtfsId Nothing anchor
        let GimsV2CurrentOperationResp {waybillNo = wbNo, active = mbActive, upcoming = ups} = op
            vehicleNumber = op.vehicleNumber
        case req.action of
          TripV2Start -> do
            let mbNext = find (\t -> t.status == "upcoming" && maybe True (== t.tripNumber) req.tripNumber) ups
            case mbNext of
              Nothing -> logWarning "FRFSFleetOperator v2: start checks skipped - no upcoming trip"
              Just next -> do
                validateStartLeadTime ctx.ibppConfig wbNo vehicleNumber next.tripNumber next.routeId (fromMaybe (Minutes 20) tc.tripStartLeadTime)
                mbFirstStop <- boundaryStopPoint ctx.ibppConfig next.routeId True
                validateWithinRadius "start" wbNo vehicleNumber req.location mbFirstStop (fromMaybe (Meters 500) tc.tripStartGeofenceRadius)
          _ -> case mbActive of
            Nothing -> logWarning "FRFSFleetOperator v2: end checks skipped - no running trip"
            Just running -> do
              mbLastStop <- boundaryStopPoint ctx.ibppConfig running.routeId False
              validateWithinRadius "end" wbNo vehicleNumber req.location mbLastStop (fromMaybe (Meters 1000) tc.tripEndGeofenceRadius)

notifyTripChangedByToken :: Id Domain.Types.Merchant.Merchant -> Text -> Flow ()
notifyTripChangedByToken merchantId token = do
  mbDriver <- QPerson.findByOperatorBadgeTokenAndMerchantId (Just token) merchantId
  case mbDriver of
    Nothing -> logWarning "FRFSFleetOperator v2: trip-change notify skipped - no driver/conductor for token"
    Just driver ->
      notifyDriverOnEvents
        driver.merchantOperatingCityId
        driver.id
        driver.deviceToken
        NotifReq {entityId = driver.id.getId, title = "Trip updated", message = "Your trip was updated. Tap to refresh."}
        FCM.TRIP_UPDATED

postFrfsFleetOperatorV2CurrentOperation ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FleetOperatorCurrentOperationV2Req ->
    Flow FleetOperatorCurrentOperationV2Resp
  )
postFrfsFleetOperatorV2CurrentOperation ctx req = postFrfsFleetOperatorV2CurrentOperation' ctx False Nothing req

postFrfsFleetOperatorV2CurrentOperation' ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Bool ->
    Maybe Text ->
    FleetOperatorCurrentOperationV2Req ->
    Flow FleetOperatorCurrentOperationV2Resp
  )
postFrfsFleetOperatorV2CurrentOperation' (mbPersonId, _merchantId, merchantOpCityId) isDashboard mbOperatorId req = do
  ctx <- transitV2Ctx merchantOpCityId
  anchor <- transitV2Anchor isDashboard mbPersonId req.gimsConductorId req.gimsDriverId req.vehicleNumber req.dutyGroupId
  op <- NandiFlow.gimsV2CurrentOperation ctx.gimsUrl ctx.gtfsId mbOperatorId anchor
  toCurrentOperationV2 ctx op

-- | GIMS run view -> driver-app response, with route number / name for display.
toCurrentOperationV2 :: TransitV2Ctx -> GimsV2CurrentOperationResp -> Flow FleetOperatorCurrentOperationV2Resp
toCurrentOperationV2 ctx op = do
  let allTrips = maybeToList op.active <> op.upcoming <> op.history
      routeIds = ordNub (map (.routeId) allTrips)
  -- Route number / name for display; best effort, cached for 12h by OTPRest.
  routes <- forM routeIds $ \rid -> do
    eRoute <- try @_ @SomeException (OTPRest.getRouteByRouteId ctx.ibppConfig rid)
    pure (rid, either (const Nothing) (\r -> r) eRoute)
  let routeMap = HashMap.fromList routes
      toInfo (t :: GimsV2TripView) =
        let mbRoute = join (HashMap.lookup t.routeId routeMap)
         in OperatorTripInfoV2
              { dutyId = t.dutyId,
                tripId = t.tripId,
                tripNumber = t.tripNumber,
                routeId = t.routeId,
                routeNumber = mbRoute >>= (.shortName),
                routeName = mbRoute >>= (.longName),
                isBookable = t.isBookable,
                scheduledStartAt = t.scheduledStartAt,
                scheduledEndAt = t.scheduledEndAt,
                recordedStartTime = t.recordedStartTime,
                recordedEndTime = t.recordedEndTime,
                driverTokenNumber = t.driverTokenNumber,
                driverName = t.driverName,
                conductorTokenNumber = t.conductorTokenNumber,
                conductorName = t.conductorName,
                status = t.status,
                cancelReason = t.cancelReason,
                skipReason = t.skipReason
              }
  pure
    FleetOperatorCurrentOperationV2Resp
      { waybillNo = op.waybillNo,
        dutyGroupId = op.dutyGroupId,
        tripGroupCode = op.tripGroupCode,
        operationDate = op.operationDate,
        gtfsId = ctx.gtfsId,
        vehicleNumber = op.vehicleNumber,
        gimsDriverId = op.driverToken,
        gimsConductorId = op.conductorToken,
        serviceTypeId = op.serviceTypeId,
        current = toInfo <$> op.active,
        upcoming = map toInfo op.upcoming,
        history = map toInfo op.history
      }

-- | v2 of 'postFrfsFleetOperatorActiveManifest': the caller's running trip from GIMS v2, falling
-- back to the client's last-known trip / route when GIMS can't resolve it.
postFrfsFleetOperatorV2ActiveManifest ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FRFSActiveManifestReq ->
    Flow FRFSActiveManifestResp
  )
postFrfsFleetOperatorV2ActiveManifest (mbPersonId, merchantId, merchantOpCityId) req = do
  ctx <- transitV2Ctx merchantOpCityId
  mbDerived <- resolveEmployeeGimsAnchor mbPersonId
  mbActiveTrip <- case mbDerived of
    Just a -> NandiFlow.gimsV2ActiveTrip ctx.gimsUrl ctx.gtfsId Nothing (GimsV2Anchor Nothing a.gimsDriverId a.gimsConductorId Nothing)
    Nothing -> pure Nothing
  let (mbTripId, mbRouteId) = case mbActiveTrip of
        Just t -> (Just t.tripId, Just t.routeId)
        Nothing -> (req.tripId, req.routeId)
  mbManifest <- case (mbTripId, mbRouteId) of
    (Just tripId, Just routeId) -> Just <$> getV2FrfsTripRouteManifest (mbPersonId, merchantId, merchantOpCityId) tripId routeId
    _ -> pure Nothing
  pure
    FRFSActiveManifestResp
      { tripId = mbTripId,
        routeId = mbRouteId,
        manifest = maybe [] (.manifest) mbManifest
      }
