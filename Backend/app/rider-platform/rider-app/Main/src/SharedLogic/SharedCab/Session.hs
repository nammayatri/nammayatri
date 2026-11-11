module SharedLogic.SharedCab.Session
  ( selectRoute,
    stranded,
    applyQueuedRoute,
    endRoute,
    pause,
    resume,
    getSession,
    getSessionFresh,
    readSession,
    CabDriverInfo (..),
    setCabDriverInfo,
    clearCabDriverInfo,
    readCabDriverInfo,
    withPlateLock,
    activeSessionsOnRoute,
    liveSessionsInCity,
    setWalkupCount,
    markCabFull,
    expire,
    dropStrandedRider,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Vector as V
import qualified Database.Redis as RawRedis
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Scheduler (JobCreator)
import SharedLogic.IntegratedBPPConfig (findSharedCabConfig)
import SharedLogic.SharedCab.Booking (ridersOnBoard, shared)
import qualified SharedLogic.SharedCab.Booking as Booking
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import SharedLogic.SharedCab.Keys (driverInfoKey, legacyRouteKey, livePlatesKey, plateLockKey, routeKey, sessionKey)
import SharedLogic.SharedCab.Lock (withOwnedLock)
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Schedule (ensureDegradedSweep, ensureExpiryJob)
import qualified SharedLogic.SharedCab.Seats as Seats
import SharedLogic.SharedCab.SessionState
import SharedLogic.SharedCab.TripClient (TripClientFlow)
import qualified SharedLogic.SharedCab.TripClient as TripClient
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import Tools.Error (SharedCabSessionError (..))

data CabDriverInfo = CabDriverInfo {driverName :: Maybe Text, vehicleModel :: Maybe Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

setCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> CabDriverInfo -> m ()
setCabDriverInfo plate info
  | isNothing info.driverName && isNothing info.vehicleModel = clearCabDriverInfo plate
  | otherwise = shared $ Redis.setExp (driverInfoKey plate) info sessionTtlSec

clearCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m ()
clearCabDriverInfo = void . shared . Redis.del . driverInfoKey

readCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe CabDriverInfo)
readCabDriverInfo = shared . Redis.safeGet . driverInfoKey

lockTtlSec :: Int
lockTtlSec = 30

withPlateLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Text -> m a -> m a
withPlateLock plate = withOwnedLock "plateLock" (plateLockKey plate) lockTtlSec 25000

-- | The plate lock on the canonical spelling of a plate; the body gets that plate.
readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = shared . Redis.safeGet . sessionKey

type GetSessionFlow m r = (Seats.SeatFlow m r, Redis.HedisLTSFlowEnv r, MonadMask m, JobCreator r m, TripClientFlow m r)

-- | After a Redis flush driver-app's ACTIVE trip restores the session (absent means the run ended); `merchantId` is the rider-app
-- merchant to ask it through. An idle driver's poll finds no trip without taking the plate lock, and that answer is kept 10 s per plate.
getSession :: GetSessionFlow m r => Id DM.Merchant -> Text -> m (Maybe Session)
getSession = getSessionWith True

-- | For a select: a cached answer must never hide the trip of a plate that is starting a run.
getSessionFresh :: GetSessionFlow m r => Id DM.Merchant -> Text -> m (Maybe Session)
getSessionFresh = getSessionWith False

getSessionWith :: GetSessionFlow m r => Bool -> Id DM.Merchant -> Text -> m (Maybe Session)
getSessionWith cached merchantId plate =
  readSession plate >>= \case
    Just s -> pure (Just s)
    Nothing -> do
      let fetch = TripClient.activeTrip merchantId plate
      mbTrip <- if cached then IM.withInMemCache ["SharedCabPlateTrip", plate] 10 fetch else fetch
      forM mbTrip $ \trip -> withPlateLock plate $ readSession plate >>= maybe (rebuild trip) pure
  where
    rebuild trip = do
      integratedBppConfig <- findSharedCabConfig trip.agencyId DIBC.APPLICATION
      s <- sessionFromTrip integratedBppConfig.merchantId integratedBppConfig.merchantOperatingCityId integratedBppConfig.id trip <$> getCurrentTime
      bookings <- Booking.liveBookingsForVehicle plate
      -- the flush took the plate's assignment index: re-seed it from the rows read, so the next seat count sees them through KV
      forM_ bookings $ \b -> Booking.noteOnPlate plate b.id
      logInfo $
        "sharedCab: rebuilt session for " <> plate <> " from driver-app trip " <> trip.id
          <> " (route "
          <> s.routeCode
          <> ", "
          <> show (length bookings)
          <> " live app bookings)"
      walkups <- Seats.setWalkups s s.walkupCount
      rebuilt <- saveSession Nothing s {walkupCount = walkups}
      ensureExpiryJob s.merchantId s.merchantOperatingCityId
      pure rebuilt

-- | The route set is an index, the session key is truth. The legacy set is shared by every city: another city's cab on the same route
-- code is live, not stale.
activeSessionsOnRoute :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DMOC.MerchantOperatingCity -> Text -> m [Session]
activeSessionsOnRoute cityId route = do
  plates <- shared $ Redis.sMembers (routeKey cityId route)
  legacyPlates <- shared $ Redis.sMembers (legacyRouteKey route)
  filter ((== cityId) . (.merchantOperatingCityId))
    <$> indexedSessions (routeMember route) (\plate -> void $ Redis.srem (routeKey cityId route) [plate] >> Redis.srem (legacyRouteKey route) [plate]) (nub (plates <> legacyPlates))

liveSessionsInCity :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DMOC.MerchantOperatingCity -> m [Session]
liveSessionsInCity cityId = do
  plates <- shared $ Redis.sMembers (livePlatesKey cityId)
  indexedSessions (cityMember cityId) (\plate -> void $ Redis.srem (livePlatesKey cityId) [plate]) plates

-- | The live sessions of a set of plates kept as an index, in one MGET. A member that doesn't read as live is re-read strictly and
-- dropped (`dropMember`) only on a positive answer, re-checked under the plate lock (not re-entrant, so no caller may hold it): a
-- failed read keeps it, and a plate that flapped back in isn't dropped.
indexedSessions :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => (SessionRead -> RouteMember) -> (Text -> m ()) -> [Text] -> m [Session]
indexedSessions classify dropMember plates = do
  raws <- shared $ Redis.mGetClusterRaw (map sessionKey plates)
  catMaybes <$> zipWithM keep plates (V.toList raws)
  where
    keep plate raw = case classify . SessionAt <$> (raw >>= A.decodeStrict) of
      Just (LiveMember s) -> pure (Just s)
      _ ->
        classify <$> readSessionStrict plate >>= \case
          LiveMember s -> pure (Just s)
          StaleMember -> Nothing <$ repair plate
          UnknownMember -> Nothing <$ logWarning ("sharedCab: session of " <> plate <> " unreadable, plate-set repair skipped")
    repair plate =
      withTryCatch "sharedCab:repairPlateSet" (withPlateLock plate (reread plate))
        >>= either (\e -> logWarning $ "sharedCab: plate-set repair of " <> plate <> " skipped: " <> show e) pure
    reread plate =
      classify <$> readSessionStrict plate >>= \case
        StaleMember -> shared (dropMember plate)
        _ -> pure ()

-- | 'readSession' answers Nothing for a missing key, a failed read and an undecodable value alike (and deletes the last); the route-set
-- repair needs them apart.
readSessionStrict :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m SessionRead
readSessionStrict plate =
  withTryCatch "sharedCab:readSessionStrict" (shared $ Redis.runWithPrefix (sessionKey plate) RawRedis.get) <&> \case
    Left _ -> SessionUnreadable
    Right Nothing -> SessionAbsent
    Right (Just raw) -> maybe SessionUnreadable SessionAt (A.decodeStrict raw)

liftSession :: MonadFlow m => Either SharedCabSessionError a -> m a
liftSession = either throwError pure

saveSession :: (Redis.HedisFlow m r, MonadFlow m) => Maybe Session -> Session -> m Session
saveSession old new = shared $ do
  Redis.setExp (sessionKey new.vehicleNumber) new sessionTtlSec
  let moves = routeSetMoves old new
  forM_ moves.removeFrom $ \route -> void $ Redis.srem (routeKey (maybe new.merchantOperatingCityId (.merchantOperatingCityId) old) route) [new.vehicleNumber]
  forM_ moves.addTo $ \route -> Redis.sAddExp (routeKey new.merchantOperatingCityId route) [new.vehicleNumber] sessionTtlSec
  if new.status /= ENDED
    then Redis.sAddExp (livePlatesKey new.merchantOperatingCityId) [new.vehicleNumber] sessionTtlSec
    else void $ Redis.srem (livePlatesKey new.merchantOperatingCityId) [new.vehicleNumber]
  pure new

-- | A run rider-app expired, told to driver-app, which owns the trip row. A failure is only logged: the driver's next call converges
-- driver-app's row on the session anyway.
expireDriverTrip :: TripClientFlow m r => Session -> m ()
expireDriverTrip s =
  withTryCatch "sharedCab:expireDriverTrip" (TripClient.expireTrip s.merchantId s.vehicleTripId)
    >>= either (\e -> logError $ "sharedCab: couldn't expire driver-app trip " <> s.vehicleTripId <> " of " <> s.vehicleNumber <> ": " <> show e) pure

switchTo :: (SessionFlow m r c, MonadMask m) => Text -> Text -> Session -> m Session
switchTo tripId newRoute s = do
  now <- getCurrentTime
  let s' = switchRoute newRoute tripId now s
  switched <- saveSession (Just s) s'
  walkups <- Seats.carryOver switched
  when (walkups /= switched.walkupCount) $ logError $ "sharedCab: " <> show walkups <> " of " <> show switched.walkupCount <> " walk-ups carried onto " <> switched.vehicleTripId
  pure switched

stranded :: [(b, Either e ())] -> [b]
stranded results = [b | (b, Left _) <- results]

-- | Every end path. Each rider drop is caught on its own: one lock timeout must not strand the others or stop the end itself.
finish :: (SessionFlow m r c, MonadMask m) => Session -> m Session
finish s = do
  onBoard <- ridersOnBoard s.vehicleNumber
  results <- forM onBoard $ \b -> (b,) <$> withTryCatch "sharedCab:dropOnFinish" (Seats.markDropped b)
  forM_ (stranded results) $ \b -> logError $ "sharedCab: rider still on board after the session ended, booking=" <> b.id.getId <> " plate=" <> s.vehicleNumber
  -- ENDED first, since the tick claims onto ACTIVE sessions only: a crash between the steps never leaves a phantom ACTIVE cab.
  ended <- saveSession (Just s) (endSession s)
  clearCabDriverInfo s.vehicleNumber
  pure ended

-- | Open a session, or change route if this driver already has one, onto driver-app's run `tripId`. A change with riders on board
-- needs a mode.
selectRoute :: Maybe SelectRouteMode -> Text -> OpenSessionReq -> Environment.Flow (Either [DFRFSTicketBooking.FRFSTicketBooking] Session)
selectRoute mode tripId req = withPlateLock plate $ do
  requireSharedCabEnabled req.merchantOperatingCityId
  prior <- readSession plate
  liftSession (planSelect req.driverId req.routeCode prior) >>= \case
    KeepRoute s -> pure (Right s)
    ChangeRoute s -> do
      onBoard <- ridersOnBoard plate
      case mode of
        _ | null onBoard -> Right <$> switchTo tripId req.routeCode s
        -- TODO: re-drop forced riders at the nearest common stop and notify them.
        Just Force -> do
          s' <- switchTo tripId req.routeCode s
          Right s' <$ mapM_ (Notify.notifyRouteChange req.routeCode) onBoard
        Just AfterLastDrop -> Right <$> saveSession (Just s) (queueRoute req.routeCode s)
        Nothing -> pure (Left onBoard)
    OpenSession -> do
      now <- getCurrentTime
      let s = newSession req tripId now prior
      opened <- saveSession prior s
      ensureExpiryJob s.merchantId s.merchantOperatingCityId
      -- Seeded here rather than in `finish` (which runs from the expiry job with no scheduler constraint); a failure must not fail the open.
      withTryCatch "sharedCabSeedSweep" (ensureDegradedSweep s.merchantId s.merchantOperatingCityId) >>= \case
        Left e -> logError $ "sharedCab: couldn't seed the degraded sweep for " <> plate <> ": " <> show e
        Right () -> pure ()
      pure (Right opened)
  where
    plate = req.vehicleNumber

-- | Applies an `afterLastDrop` route change once no rider is left; True when it switched (the caller then releases the old route's allocations, outside this lock).
-- rider-app names this run itself; driver-app adopts it from the next session it sees.
applyQueuedRoute :: (SessionFlow m r c, MonadMask m) => Text -> m Bool
applyQueuedRoute plate = withPlateLock plate $ do
  mbSession <- readSession plate
  case mbSession of
    Just s | Just queued <- s.queuedRouteCode,
             s.status /= ENDED -> do
      onBoard <- ridersOnBoard plate
      if null onBoard then generateGUIDText >>= \tripId -> True <$ switchTo tripId queued s else pure False
    _ -> pure False

endRoute :: Text -> Text -> Bool -> EndRouteAction -> Environment.Flow Session
endRoute driver plate forced action = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  onBoard <- ridersOnBoard plate
  -- Riders aboard: only a forced return trip takes them along; ending the run is refused, the driver drops each one first.
  let returning = case action of StartReturn _ -> True; _ -> False
  unless (null onBoard || (forced && returning)) $ throwError (RidersOnBoard (length onBoard))
  case action of
    StartReturn tripId -> do
      returnRoute <- liftSession (returnRouteOf s.routeCode)
      s' <- switchTo tripId returnRoute s
      s' <$ mapM_ (Notify.notifyRouteChange returnRoute) onBoard
    _ -> finish s

-- | The silent cab's run ends here, not by the driver: driver-app hears of it after the plate lock.
expire :: (SessionFlow m r c, MonadMask m, TripClientFlow m r) => Text -> m Session
expire plate = do
  (prior, ended) <-
    withPlateLock plate $
      readSession plate >>= \case
        Just s | s.status /= ENDED -> (s,) <$> finish s
        _ -> throwError SessionNotFound
  ended <$ expireDriverTrip prior

-- | System-initiated: no driver check. Pause is session state only; a flush brings the run back ACTIVE.
pause :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> PauseReason -> m Session
pause plate reason = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ maybe (Left SessionNotFound) (pauseSession reason) prior
  saveSession prior s

resume :: Text -> Text -> Environment.Flow Session
resume driver plate = do
  resumed <- withPlateLock plate $ do
    prior <- readSession plate
    owned <- liftSession $ ownedSession driver prior
    requireSharedCabEnabled owned.merchantOperatingCityId
    s <- liftSession $ resumeSession owned
    saveSession prior s
  resumed <$ ensureExpiryJob resumed.merchantId resumed.merchantOperatingCityId

setWalkupCount :: Text -> Text -> Int -> Int -> Environment.Flow Session
setWalkupCount driver plate expectedVersion count = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior
  requireSharedCabEnabled s.merchantOperatingCityId
  s' <- liftSession $ setWalkup expectedVersion count s
  held <- Seats.setWalkups s count
  saveSession prior s' {walkupCount = held}

-- | Cab full: `releaseUnboarded` (the unboarded allocations going) runs under the plate lock, then walk-ups take every seat left,
-- so no claim lands in between. Its result comes back for the caller to send the releases' effects outside the lock.
markCabFull :: Text -> Text -> Environment.Flow a -> Environment.Flow (Session, a)
markCabFull driver plate releaseUnboarded = do
  (full, released) <- withPlateLock plate $ do
    prior <- readSession plate
    s <- liftSession $ ownedSession driver prior
    released <- releaseUnboarded
    held <- Seats.setWalkups s s.capacity
    (,released) <$> saveSession prior (fillCab held s)
  pure (full, released)

-- | End a rider `finish` couldn't drop. Plate lock serialises with `finish` and re-open; plate -> booking lock order.
dropStrandedRider :: (TripClientFlow m r, MonadMask m) => DFRFSTicketBooking.FRFSTicketBooking -> m Bool
dropStrandedRider booking = case booking.vehicleNumber of
  Nothing -> pure False
  Just plate ->
    ifStranded (isStranded plate booking) . withPlateLock plate $ do
      fresh <- QBooking.findById booking.id
      case fresh of
        Just b | b.vehicleNumber == booking.vehicleNumber -> ifStranded (isStranded plate b) (True <$ Seats.markDropped b)
        _ -> pure False
  where
    ifStranded cond act = cond >>= \c -> if c then act else pure False

isStranded :: TripClientFlow m r => Text -> DFRFSTicketBooking.FRFSTicketBooking -> m Bool
isStranded plate b
  | b.status /= DBookingStatus.CONFIRMED = pure False
  | otherwise = do
    mbSession <- readSession plate
    -- an unreachable driver-app counts as a live trip: riders are never dropped for it
    hasLiveTrip <- if isNothing mbSession then either (const True) isJust <$> withTryCatch "sharedCab:strandedTripCheck" (TripClient.activeTrip b.merchantId plate) else pure False
    statuses <- map (.status) <$> QTicket.findAllByTicketBookingId b.id
    pure $ shouldDropOnDeadSession ((.status) <$> mbSession) hasLiveTrip statuses
