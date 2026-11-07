module Domain.Action.UI.OpenMeter
  ( postOpenMeterRideStart,
    postOpenMeterRideHeartbeat,
    postOpenMeterRideEnd,
    postOpenMeterRideCancel,
    getOpenMeterRide,
    getOpenMeterRides,
    deleteOpenMeterRide,
    autoEndRunningRides,
  )
where

import qualified API.Types.UI.OpenMeter as API
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OpenMeterRide as DOMR
import qualified Domain.Types.Person as DP
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.OpenMeter as SOM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.OpenMeterRide as QOMR
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | Start is serialised per rider, so two starts can never both see "no RUNNING ride" (KV
-- inserts cannot enforce uniqueness). Retrying a start with the same rideId returns the ride.
-- Every read under an open meter lock goes to the master DB and Redis: a replica lagging the
-- last write could hide a RUNNING ride from a start, or hand a transition stale totals.
postOpenMeterRideStart ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  API.OpenMeterRideStartReq ->
  Flow API.OpenMeterRideRes
postOpenMeterRideStart (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  unless (SOM.isValidRideId req.rideId) $ throwError $ InvalidRequest "rideId must be a UUID"
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
  let allowMultiple = maybe False (.allowMultipleOpenMeterRides) riderConfig
      maxIdleHours = maybe 12 (.openMeterMaxIdleHours) riderConfig
  ride <- withOpenMeterLock (personLockKey personId) $ do
    now <- getCurrentTime
    mbExisting <- B.runInMasterDbAndRedis $ QOMR.findByPrimaryKey req.rideId
    running <- B.runInMasterDbAndRedis $ QOMR.findAllByPersonIdAndStatus personId DOMR.RUNNING
    case SOM.decideStart personId allowMultiple now maxIdleHours mbExisting running of
      SOM.ReturnExisting existing -> pure existing
      SOM.RejectNotFound -> throwError $ OpenMeterRideNotFound req.rideId.getId
      SOM.RejectActive activeRideId -> throwError $ OpenMeterRideAlreadyActive activeRideId.getId
      SOM.Create staleRideIds -> do
        forM_ staleRideIds $ autoEndStaleRide now maxIdleHours
        let newRide =
              DOMR.OpenMeterRide
                { id = req.rideId,
                  personId = personId,
                  status = DOMR.RUNNING,
                  startTime = req.startTime,
                  serverStartedAt = now,
                  endTime = Nothing,
                  lastHeartbeatAt = Nothing,
                  startLatLong = req.startLatLong,
                  endLatLong = Nothing,
                  lastLatLong = Nothing,
                  distanceMeters = Meters 0,
                  serverLinearDistanceMeters = Nothing,
                  estimatedGapMeters = Meters 0,
                  waitingTimeSeconds = Seconds 0,
                  encodedPolyline = Nothing,
                  pointsCount = 0,
                  fare = Nothing,
                  currency = Nothing,
                  tariffSnapshot = req.tariffSnapshot,
                  trackingConfig = req.trackingConfig,
                  endReason = Nothing,
                  merchantId = Just person.merchantId,
                  merchantOperatingCityId = Just person.merchantOperatingCityId,
                  createdAt = now,
                  updatedAt = now
                }
        QOMR.create newRide
        pure newRide
  pure $ mkOpenMeterRideRes False ride

postOpenMeterRideHeartbeat ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DOMR.OpenMeterRide ->
  API.OpenMeterRideHeartbeatReq ->
  Flow API.OpenMeterRideRes
postOpenMeterRideHeartbeat (mbPersonId, _merchantId) rideId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  whenJust (SOM.negativeHeartbeatField req) $ throwError . mustNotBeNegative
  ride <- withOpenMeterLock (rideLockKey rideId) $ do
    stored <- findOwnedRide personId rideId
    now <- getCurrentTime
    commitTransition stored (SOM.applyHeartbeat now req stored) $ \updated ->
      QOMR.updateHeartbeatById updated.distanceMeters updated.waitingTimeSeconds updated.lastLatLong updated.fare updated.lastHeartbeatAt rideId
  pure $ mkOpenMeterRideRes False ride

postOpenMeterRideEnd ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DOMR.OpenMeterRide ->
  API.OpenMeterRideEndReq ->
  Flow API.OpenMeterRideRes
postOpenMeterRideEnd (mbPersonId, _merchantId) rideId req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  whenJust (SOM.negativeEndField req) $ throwError . mustNotBeNegative
  serverLinear <- SOM.serverLinearDistance req.encodedPolyline & fromMaybeM (InvalidRequest "encodedPolyline is not a valid encoded polyline")
  ride <- withOpenMeterLock (rideLockKey rideId) $ do
    stored <- findOwnedRide personId rideId
    now <- getCurrentTime
    commitTransition stored (SOM.applyEnd now req serverLinear stored) $ \ended -> do
      when (SOM.isDistanceMismatch ended.distanceMeters serverLinear) $
        logWarning $
          "Open meter ride " <> rideId.getId <> ": client distance " <> show ended.distanceMeters.getMeters
            <> " m is more than 30% off the server's linear distance "
            <> show serverLinear.getMeters
            <> " m"
      QOMR.updateEndById ended.status ended.endTime ended.endLatLong ended.distanceMeters ended.serverLinearDistanceMeters ended.waitingTimeSeconds ended.fare ended.currency ended.encodedPolyline ended.pointsCount ended.estimatedGapMeters ended.endReason rideId
  pure $ mkOpenMeterRideRes False ride

postOpenMeterRideCancel ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DOMR.OpenMeterRide ->
  Flow API.OpenMeterRideRes
postOpenMeterRideCancel (mbPersonId, _merchantId) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ride <- withOpenMeterLock (rideLockKey rideId) $ do
    stored <- findOwnedRide personId rideId
    now <- getCurrentTime
    commitTransition stored (SOM.applyCancel now stored) $ \cancelled ->
      QOMR.updateStatusById cancelled.status rideId
  pure $ mkOpenMeterRideRes False ride

getOpenMeterRide ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DOMR.OpenMeterRide ->
  Flow API.OpenMeterRideRes
getOpenMeterRide (mbPersonId, _merchantId) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mkOpenMeterRideRes True <$> findOwnedRide personId rideId

getOpenMeterRides ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Maybe Int ->
  Maybe Int ->
  Maybe DOMR.OpenMeterRideStatus ->
  Flow [API.OpenMeterRideRes]
getOpenMeterRides (mbPersonId, _merchantId) mbLimit mbOffset mbStatus = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  map (mkOpenMeterRideRes False) <$> QOMR.findAllByPersonIdPaginated personId mbStatus mbLimit mbOffset

deleteOpenMeterRide ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DOMR.OpenMeterRide ->
  Flow APISuccess.APISuccess
deleteOpenMeterRide (mbPersonId, _merchantId) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  withOpenMeterLock (rideLockKey rideId) $ do
    ride <- findOwnedRide personId rideId
    when (ride.status == DOMR.RUNNING) $ throwError $ OpenMeterRideInvalidState (show ride.status)
    QOMR.deleteById rideId
  pure APISuccess.Success

-- | Auto-ends a ride that decideStart found stale. It takes the ride's own lock and re-reads
-- the ride, so it cannot overwrite a concurrent end or cancel. If a heartbeat revived the ride
-- meanwhile, the start is refused as busy and the client's retry decides on fresh state.
autoEndStaleRide :: UTCTime -> Int -> Id DOMR.OpenMeterRide -> Flow ()
autoEndStaleRide now maxIdleHours rideId =
  withOpenMeterLock (rideLockKey rideId) $
    B.runInMasterDbAndRedis (QOMR.findByPrimaryKey rideId) >>= \case
      Just ride
        | SOM.isStale now maxIdleHours ride -> mapM_ writeAutoEnd (SOM.applyAutoEnd now ride)
        | ride.status == DOMR.RUNNING -> throwError OpenMeterRideBusy
      _ -> pure ()

-- | Logout, and a login that ends the rider's other sessions, leave no device that can end the
-- rider's RUNNING rides: a phone drops its meters and its pending sync with its session. Left
-- RUNNING, such a ride would refuse every new start of the rider with ALREADY_ACTIVE until it
-- went stale (openMeterMaxIdleHours), so each one is AUTO_ENDED here at once, under its own lock.
-- Best effort: it never fails the logout or login. A ride's lock that a heartbeat holds is waited
-- for briefly. A ride it still cannot close (lock busy, a read or a write failing) is logged and
-- left to the stale auto-end on a later start.
autoEndRunningRides :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> m ()
autoEndRunningRides personId =
  logFailure "list running rides" $ do
    running <- B.runInMasterDbAndRedis $ QOMR.findAllByPersonIdAndStatus personId DOMR.RUNNING
    forM_ running $ \ride ->
      logFailure ("ride " <> ride.id.getId) . withOpenMeterLockWaiting (rideLockKey ride.id) $ do
        now <- getCurrentTime
        B.runInMasterDbAndRedis (QOMR.findByPrimaryKey ride.id) >>= \case
          Just current | current.personId == personId -> mapM_ writeAutoEnd (SOM.applyAutoEnd now current)
          _ -> pure ()
  where
    logFailure step action =
      withTryCatch "autoEndRunningOpenMeterRides" action >>= \case
        Left err -> logError $ "Open meter auto-end for person " <> personId.getId <> " failed at " <> step <> ": " <> show err
        Right () -> pure ()

writeAutoEnd :: (CacheFlow m r, EsqDBFlow m r) => DOMR.OpenMeterRide -> m ()
writeAutoEnd ended =
  QOMR.updateEndById ended.status ended.endTime ended.endLatLong ended.distanceMeters ended.serverLinearDistanceMeters ended.waitingTimeSeconds ended.fare ended.currency ended.encodedPolyline ended.pointsCount ended.estimatedGapMeters ended.endReason ended.id

-- | Throws a retryable OPEN_METER_RIDE_BUSY when another request holds the lock.
withOpenMeterLock :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m a -> m a
withOpenMeterLock key action =
  Redis.whenWithLockRedisAndReturnValue key 10 action >>= either (const $ throwError OpenMeterRideBusy) pure

-- | withOpenMeterLock for the server's own cleanup, which has no client to retry it: a busy lock
-- is tried again every 100 ms, five times in all, before OPEN_METER_RIDE_BUSY. A heartbeat holds a
-- ride's lock only for one read and one write.
withOpenMeterLockWaiting :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m a -> m a
withOpenMeterLockWaiting key action = go (5 :: Int)
  where
    go attemptsLeft =
      Redis.whenWithLockRedisAndReturnValue key 10 action >>= \case
        Right result -> pure result
        Left ()
          | attemptsLeft > 1 -> threadDelayMilliSec 100 >> go (attemptsLeft - 1)
          | otherwise -> throwError OpenMeterRideBusy

mustNotBeNegative :: Text -> GenericError
mustNotBeNegative field = InvalidRequest (field <> " must not be negative")

personLockKey :: Id DP.Person -> Text
personLockKey personId = "OpenMeter:Person:" <> personId.getId

rideLockKey :: Id DOMR.OpenMeterRide -> Text
rideLockKey rideId = "OpenMeter:Ride:" <> rideId.getId

-- | Another rider's ride is reported exactly like a missing one. Every caller but the unlocked
-- GET holds the ride's lock, so the read goes to the master DB and Redis.
findOwnedRide :: Id DP.Person -> Id DOMR.OpenMeterRide -> Flow DOMR.OpenMeterRide
findOwnedRide personId rideId =
  B.runInMasterDbAndRedis (QOMR.findByPrimaryKey rideId) >>= \case
    Just ride | ride.personId == personId -> pure ride
    _ -> throwError $ OpenMeterRideNotFound rideId.getId

-- | Writes a transition's result, or returns the stored ride when the transition is a no-op.
commitTransition ::
  DOMR.OpenMeterRide ->
  Either OpenMeterError (Maybe DOMR.OpenMeterRide) ->
  (DOMR.OpenMeterRide -> Flow ()) ->
  Flow DOMR.OpenMeterRide
commitTransition stored transition write = case transition of
  Left err -> throwError err
  Right Nothing -> pure stored
  Right (Just updated) -> write updated $> updated

-- | encodedPolyline is only sent by GET /openMeter/ride/{rideId}.
mkOpenMeterRideRes :: Bool -> DOMR.OpenMeterRide -> API.OpenMeterRideRes
mkOpenMeterRideRes withPolyline ride =
  API.OpenMeterRideRes
    { id = ride.id,
      status = ride.status,
      startTime = ride.startTime,
      endTime = ride.endTime,
      distanceMeters = ride.distanceMeters,
      serverLinearDistanceMeters = ride.serverLinearDistanceMeters,
      waitingTimeSeconds = ride.waitingTimeSeconds,
      fare = ride.fare,
      currency = ride.currency,
      encodedPolyline = if withPolyline then ride.encodedPolyline else Nothing,
      pointsCount = ride.pointsCount,
      estimatedGapMeters = ride.estimatedGapMeters,
      createdAt = ride.createdAt
    }
