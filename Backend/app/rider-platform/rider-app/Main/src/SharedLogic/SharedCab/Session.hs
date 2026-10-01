module SharedLogic.SharedCab.Session
  ( selectRoute,
    endDropBy,
    stranded,
    changeRoute,
    applyQueuedRoute,
    endRoute,
    pause,
    resume,
    getSession,
    readSession,
    CabDriverInfo (..),
    setCabDriverInfo,
    clearCabDriverInfo,
    readCabDriverInfo,
    withPlateLock,
    activeSessionsOnRoute,
    setWalkupCount,
    markCabFull,
    expire,
    dropStrandedRider,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler (JobCreator)
import SharedLogic.SharedCab.Booking (ridersOnBoard, shared)
import qualified SharedLogic.SharedCab.Booking as Booking
import SharedLogic.SharedCab.DegradedSweepSchedule (ensureDegradedSweep)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.ExpirySchedule (ensureExpiryJob)
import SharedLogic.SharedCab.LtsAttach
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error (SharedCabSessionError (..))

sessionKey :: Text -> Text
sessionKey plate = "sharedcab:session:" <> plate

routeKey :: Text -> Text
routeKey routeCode = "sharedcab:route:" <> routeCode

-- | What the driver-app knows about the cab's driver, handed over once at route select (so no per-poll call).
-- Display only, so it lives beside the session with the session's lifetime, not inside it.
data CabDriverInfo = CabDriverInfo {driverName :: Maybe Text, vehicleModel :: Maybe Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

driverInfoKey :: Text -> Text
driverInfoKey plate = "sharedcab:driverinfo:" <> plate

-- | Written on every route select, so a new driver on the plate (or an older app that sends nothing) never shows the
-- previous driver: an empty info removes the key.
setCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> CabDriverInfo -> m ()
setCabDriverInfo plate info
  | isNothing info.driverName && isNothing info.vehicleModel = clearCabDriverInfo plate
  | otherwise = shared $ Redis.setExp (driverInfoKey plate) info sessionTtlSec

clearCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m ()
clearCabDriverInfo = void . shared . Redis.del . driverInfoKey

readCabDriverInfo :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe CabDriverInfo)
readCabDriverInfo = shared . Redis.safeGet . driverInfoKey

lockKey :: Text -> Text
lockKey plate = "sharedcab:lock:" <> plate

-- rider_config.maxSessionHours default; read from config once that field exists.
sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

-- Covers the LTS calls made under the lock.
lockTtlSec :: Int
lockTtlSec = 30

withPlateLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Text -> m a -> m a
withPlateLock plate = Redis.withWaitAndLockMasterCloudCrossAppRedis "sharedCab" "plateLock" (lockKey plate) lockTtlSec 25000

readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = shared . Redis.safeGet . sessionKey

-- | After a Redis flush the plate's live vehicle_trip restores the session (§3a) and re-lists it on its route;
-- an ACTIVE or PAUSED row recovers, an absent one means the run ended — no session. The plate's non-terminal
-- bookings keep joining by plate on their own; they're counted into the log so a rebuilt session can be told
-- apart from one that lost riders. Only the `saveSession` write makes the session downstream-visible.
getSession :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m, JobCreator r m) => Text -> m (Maybe Session)
getSession rawPlate =
  readSession plate >>= \case
    Just s -> pure (Just s)
    Nothing -> withPlateLock plate $ readSession plate >>= maybe recover (pure . Just)
  where
    plate = canonicalisePlate rawPlate
    recover = QVT.findActiveByVehicleNumber plate >>= traverse rebuild
    rebuild trip = do
      s <- sessionFromTrip trip <$> getCurrentTime
      bookings <- Booking.liveBookingsForVehicle plate
      logInfo $
        "sharedCab: rebuilt session for " <> plate <> " from vehicle_trip " <> trip.id.getId
          <> " (route "
          <> s.routeCode
          <> ", "
          <> show (length bookings)
          <> " live app bookings)"
      rebuilt <- saveSession Nothing s
      ensureExpiryJob s.merchantId s.merchantOperatingCityId
      pure rebuilt

-- | Each member is re-read: a route set can outlive a session that has since paused or ended.
-- R21: the route set is an index, the session key is truth. A member is kept only while its session is ACTIVE
-- and still on this route; anything else is stale (a crash between `saveSession`'s session write and set moves, or
-- an expired key) and is repaired lazily: the session is re-read once, and the SREM is skipped only if the plate
-- meanwhile flapped back onto this route ACTIVE, so an A->B->A move can't be undone by a stale read. The re-read and
-- SREM run under the plate lock (no caller holds one, and it isn't re-entrant), so a switch's SADD can't slip between
-- them; a repair that can't get the lock is skipped, the next read retries.
activeSessionsOnRoute :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> m [Session]
activeSessionsOnRoute route = do
  plates <- shared $ Redis.sMembers (routeKey route)
  catMaybes <$> mapM keepOrRepair plates
  where
    keepOrRepair plate =
      readSession plate >>= \case
        Just s | s.status == ACTIVE && s.routeCode == route -> pure (Just s)
        _ -> repair plate >> pure Nothing
    repair plate =
      withTryCatch "sharedCab:repairRouteSet" (withPlateLock plate (reread plate))
        >>= either (\e -> logWarning $ "sharedCab: route-set repair of " <> plate <> " on " <> route <> " skipped: " <> show e) pure
    reread plate =
      readSession plate >>= \case
        Just s | s.status == ACTIVE && s.routeCode == route -> pure ()
        _ -> void $ shared $ Redis.srem (routeKey route) [plate]

liftSession :: MonadFlow m => Either SharedCabSessionError a -> m a
liftSession = either throwError pure

saveSession :: (Redis.HedisFlow m r, MonadFlow m) => Maybe Session -> Session -> m Session
saveSession old new = shared $ do
  Redis.setExp (sessionKey new.vehicleNumber) new sessionTtlSec
  let moves = routeSetMoves old new
  forM_ moves.removeFrom $ \route -> void $ Redis.srem (routeKey route) [new.vehicleNumber]
  forM_ moves.addTo $ \route -> Redis.sAddExp (routeKey route) [new.vehicleNumber] sessionTtlSec
  pure new

-- Closes whatever trip the DB holds live for the plate, not just the session's, so a stale row can't block the next open.
closeLiveTrip :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> DVT.VehicleTripEndReason -> UTCTime -> m ()
closeLiveTrip plate reason now =
  QVT.findActiveByVehicleNumber plate
    >>= traverse_ (\trip -> QVT.closeTrip (closedTripStatus reason) (Just reason) (Just now) trip.id)

-- | LTS moves before anything is persisted; if it fails, the select/change fails and the session stays as it was.
switchTo :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => DVT.VehicleTripEndReason -> Text -> Session -> m Session
switchTo reason newRoute s = do
  now <- getCurrentTime
  tripId <- generateGUID
  let s' = switchRoute newRoute tripId s
  withAttach (Just s) s' (replaceLiveTrip reason now (Just s) s') <* Events.forSession (Events.RouteChanged s.routeCode) s'

-- | The live-trip index (1575) allows one ACTIVE/PAUSED row per plate, so the old run closes before the new one is
-- created. If creating or saving then fails, the new row (if any) is abandoned and the old one reopened.
replaceLiveTrip :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadCatch m) => DVT.VehicleTripEndReason -> UTCTime -> Maybe Session -> Session -> m Session
replaceLiveTrip reason now prior s' = do
  mbOld <- QVT.findActiveByVehicleNumber s'.vehicleNumber
  whenJust mbOld $ \old -> QVT.closeTrip (closedTripStatus reason) (Just reason) (Just now) old.id
  (QVT.create (tripFor s' now) >> saveSession prior s') `onException` undo mbOld
  where
    undo mbOld =
      withTryCatch
        "sharedCab:undoTripSwitch"
        ( do
            QVT.findById s'.vehicleTripId >>= traverse_ (\new -> QVT.closeTrip DVT.ABANDONED (Just DVT.ROUTE_CHANGED) (Just now) new.id)
            whenJust mbOld $ \old -> QVT.closeTrip old.status Nothing Nothing old.id
        )
        >>= either (\e -> logError $ "sharedCab: couldn't undo the trip switch for " <> s'.vehicleNumber <> ": " <> show e) pure

endDropBy :: DVT.VehicleTripEndReason -> Events.DropBy
endDropBy = \case
  DVT.SESSION_TIMEOUT -> Events.DroppedByTick
  DVT.OPS_FORCED -> Events.DroppedByTick
  _ -> Events.DroppedByDriver

-- | The riders whose drop failed: still INPROGRESS after `finish`.
stranded :: [(b, Either e ())] -> [b]
stranded results = [b | (b, Left _) <- results]

-- | Every end path: drop the riders still on board, close the trip, end the session, then take the cab off its LTS route.
-- The drops come first (booking lock inside the plate lock), each caught on its own: one lock timeout must not strand the
-- others, and must not stop the end itself -- the driver or the expiry job could then never end the run. A rider whose
-- drop failed is logged and stays INPROGRESS on the ended session; the rider's own "I got down" (markDropped) still ends it.
-- (Dropping after saving ENDED lost them all on the first failure: an ENDED session is never stepped again.)
finish :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => DVT.VehicleTripEndReason -> Session -> m Session
finish reason s = do
  onBoard <- ridersOnBoard s.vehicleNumber
  results <- forM onBoard $ \b -> (b,) <$> withTryCatch "sharedCab:dropOnFinish" (Booking.markDropped (endDropBy reason) b)
  forM_ (stranded results) $ \b -> logError $ "sharedCab: rider still on board after the session ended, booking=" <> b.id.getId <> " plate=" <> s.vehicleNumber
  now <- getCurrentTime
  -- R67: the session goes ENDED first, since the tick claims onto ACTIVE sessions only. A crash between the steps then leaves a
  -- stale live trip row (the next open closes it, closeLiveTrip) or a cab still on its LTS route (the tick ignores it),
  -- never a phantom ACTIVE cab that keeps taking bookings.
  ended <- saveSession (Just s) (endSession s)
  closeLiveTrip s.vehicleNumber reason now
  clearCabDriverInfo s.vehicleNumber
  detach s
  ended <$ Events.forSession (Events.Ended (show reason)) s

-- | Open a session, or change route if this driver already has one (idempotent for the same route).
-- A change with riders on board (`04` §4) needs a mode: Left lists them when there is none.
selectRoute ::
  (ServiceFlow m r, Redis.HedisFlow m r, LtsFlow m r c, Events.EventFlow m r, MonadMask m, JobCreator r m) =>
  Maybe SelectRouteMode ->
  OpenSessionReq ->
  m (Either [DFRFSTicketBooking.FRFSTicketBooking] Session)
selectRoute mode req = withPlateLock plate $ do
  prior <- readSession plate
  liftSession (planSelect req.driverId req.routeCode prior) >>= \case
    KeepRoute s -> pure (Right s)
    ChangeRoute s -> do
      onBoard <- ridersOnBoard plate
      case mode of
        _ | null onBoard -> Right <$> switchTo DVT.ROUTE_CHANGED req.routeCode s
        -- TODO(B12): re-drop forced riders at the nearest common stop and notify them.
        Just Force -> do
          s' <- switchTo DVT.ROUTE_CHANGED req.routeCode s
          Right s' <$ mapM_ (Notify.notifyRouteChange req.routeCode) onBoard
        Just AfterLastDrop -> Right <$> saveSession (Just s) (queueRoute req.routeCode s)
        Nothing -> pure (Left onBoard)
    OpenSession -> do
      now <- getCurrentTime
      tripId <- generateGUID
      let s = newSession req tripId now prior
      opened <- withAttach Nothing s $ replaceLiveTrip DVT.SESSION_TIMEOUT now prior s
      ensureExpiryJob s.merchantId s.merchantOperatingCityId
      -- R51: a rider `finish` can't drop is only ever swept if the city has a chain, and a city with sessions has them. Seeded
      -- here (idempotent, and it heals a lapsed chain) rather than in `finish`, which runs from the expiry job and the API
      -- with no scheduler constraint; a failure must not fail the open.
      withTryCatch "sharedCabSeedSweep" (ensureDegradedSweep s.merchantId s.merchantOperatingCityId) >>= \case
        Left e -> logError $ "sharedCab: couldn't seed the degraded sweep for " <> plate <> ": " <> show e
        Right () -> pure ()
      Events.forSession Events.SessionStarted opened
      pure (Right opened)
  where
    plate = canonicalisePlate req.vehicleNumber

changeRoute :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => Text -> Text -> Text -> m Session
changeRoute driver rawPlate newRoute = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  if s.routeCode == newRoute then pure s else switchTo DVT.ROUTE_CHANGED newRoute s
  where
    plate = canonicalisePlate rawPlate

-- | Call after each drop: applies an `afterLastDrop` route change once no rider is left on board.
-- True when it switched: the caller then releases the old route's unboarded allocations, outside this lock.
applyQueuedRoute :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => Text -> m Bool
applyQueuedRoute rawPlate = withPlateLock plate $ do
  mbSession <- readSession plate
  case mbSession of
    Just s | Just queued <- s.queuedRouteCode,
             s.status /= ENDED -> do
      onBoard <- ridersOnBoard plate
      if null onBoard then True <$ switchTo DVT.ROUTE_CHANGED queued s else pure False
    _ -> pure False
  where
    plate = canonicalisePlate rawPlate

-- | Refused while riders are on board unless `forced`: `finish` then drops them. A return trip is a route change, riders are told.
endRoute :: (ServiceFlow m r, LtsFlow m r c, Events.EventFlow m r, MonadMask m) => Text -> Text -> Bool -> EndRouteAction -> m Session
endRoute driver rawPlate forced action = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  onBoard <- ridersOnBoard plate
  unless (forced || null onBoard) $ throwError (RidersOnBoard (length onBoard))
  case action of
    StartReturn -> do
      returnRoute <- liftSession (returnRouteOf s.routeCode)
      s' <- switchTo DVT.RETURN returnRoute s
      s' <$ mapM_ (Notify.notifyRouteChange returnRoute) onBoard
    _ -> do
      finish (endActionReason action) s
  where
    plate = canonicalisePlate rawPlate

-- | System end (expiry job, ops): no driver check.
expire :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => Text -> m Session
expire rawPlate =
  withPlateLock plate $
    readSession plate >>= \case
      Just s | s.status /= ENDED -> finish DVT.SESSION_TIMEOUT s
      _ -> throwError SessionNotFound
  where
    plate = canonicalisePlate rawPlate

-- | System-initiated (tick, offline toggle): no driver check. Mirrored to the trip row so flush recovery
-- restores PAUSED rather than silently resuming.
pause :: (Events.EventFlow m r, MonadMask m) => Text -> PauseReason -> m Session
pause rawPlate reason = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ maybe (Left SessionNotFound) (pauseSession reason) prior
  QVT.updateStatus DVT.PAUSED s.vehicleTripId
  saveSession prior s <* Events.forSession (Events.Paused (show reason)) s
  where
    plate = canonicalisePlate rawPlate

resume :: (Events.EventFlow m r, MonadMask m, JobCreator r m) => Text -> Text -> m Session
resume driver rawPlate = do
  resumed <- withPlateLock plate $ do
    prior <- readSession plate
    s <- liftSession $ ownedSession driver prior >>= resumeSession
    QVT.updateStatus DVT.ACTIVE s.vehicleTripId
    saveSession prior s
  Events.forSession Events.Resumed resumed
  resumed <$ ensureExpiryJob resumed.merchantId resumed.merchantOperatingCityId
  where
    plate = canonicalisePlate rawPlate

-- | Compare-and-set on `version`; each walk-up added is also counted on the trip row (offlineBoardings).
setWalkupCount :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Int -> Int -> m Session
setWalkupCount driver rawPlate expectedVersion count = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior
  s' <- liftSession $ setWalkup expectedVersion count s
  saveWalkups DriverCounted prior s s'
  where
    plate = canonicalisePlate rawPlate

-- | R19 driver "cab full": walk-ups fill every seat the boarded riders don't hold. Written under the plate lock
-- BEFORE the caller releases the unboarded allocations (SEAT_LOST), so while they are still counted `available`
-- is below zero and no claim can land on the cab in between.
markCabFull :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m, Events.EventFlow m r) => Text -> Text -> m Session
markCabFull driver rawPlate = do
  full <- withPlateLock plate $ do
    prior <- readSession plate
    s <- liftSession $ ownedSession driver prior
    boarded <- Booking.boardedSeatsOnVehicle plate
    saveWalkups CabFullFill prior s (fillCab boarded s)
  Events.forSession (Events.CabFull full.walkupCount) full
  pure full
  where
    plate = canonicalisePlate rawPlate

-- | Walk-ups the driver counted are also counted on the trip row (offlineBoardings); a cab-full fill is not (R24: the
-- CabFull event records it).
saveWalkups :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => WalkupSource -> Maybe Session -> Session -> Session -> m Session
saveWalkups source prior s s' = do
  let added = walkupsToCount source s s'
  when (added > 0) $
    QVT.findById s.vehicleTripId
      >>= traverse_ (\trip -> QVT.updateOfflineBoardings (trip.offlineBoardings + added) trip.id)
  saveSession prior s'

-- | R51: end a rider `finish` couldn't drop. True when this call dropped it. The plate lock serialises with `finish` and a
-- re-open; the booking is re-decided on a fresh read inside it (plate -> booking lock order, `markDropped` takes the latter).
dropStrandedRider :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m, Events.EventFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> m Bool
dropStrandedRider booking = case canonicalisePlate <$> booking.vehicleNumber of
  Nothing -> pure False
  Just plate ->
    ifStranded (isStranded plate booking) . withPlateLock plate $ do
      fresh <- QBooking.findById booking.id
      case fresh of
        Just b | b.vehicleNumber == booking.vehicleNumber -> ifStranded (isStranded plate b) (True <$ Booking.markDropped Events.DroppedByTick b)
        _ -> pure False
  where
    ifStranded cond act = cond >>= \c -> if c then act else pure False
    isStranded plate b
      | b.status /= DBookingStatus.CONFIRMED = pure False
      | otherwise = do
        mbSession <- readSession plate
        hasLiveTrip <- if isNothing mbSession then isJust <$> QVT.findActiveByVehicleNumber plate else pure False
        statuses <- map (.status) <$> QTicket.findAllByTicketBookingId b.id
        pure $ shouldDropOnDeadSession ((.status) <$> mbSession) hasLiveTrip statuses
