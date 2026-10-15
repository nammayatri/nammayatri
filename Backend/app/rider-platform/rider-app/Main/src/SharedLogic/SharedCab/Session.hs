module SharedLogic.SharedCab.Session
  ( selectRoute,
    endDropBy,
    stranded,
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
    filterCabLegs,
    setWalkupCount,
    markCabFull,
    expire,
    dropStrandedRider,
  )
where

import Control.Monad.Extra (allM)
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.VehicleTrip as DVT
import qualified Environment
import qualified Kernel.External.MultiModal.Interface.Types as MultiModalTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler (JobCreator)
import SharedLogic.SharedCab.Booking (ridersOnBoard, shared)
import qualified SharedLogic.SharedCab.Booking as Booking
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.Keys (driverInfoKey, plateLockKey, routeKey, sessionKey)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import SharedLogic.SharedCab.LtsAttach
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.Schedule (ensureDegradedSweep, ensureExpiryJob)
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error (GenericError (InvalidRequest), SharedCabSessionError (..))

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

sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

lockTtlSec :: Int
lockTtlSec = 30

withPlateLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Text -> m a -> m a
withPlateLock plate = Redis.withWaitAndLockMasterCloudCrossAppRedis "sharedCab" "plateLock" (plateLockKey plate) lockTtlSec 25000

readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = shared . Redis.safeGet . sessionKey

-- | After a Redis flush the live vehicle_trip restores the session (ACTIVE or PAUSED recovers, absent means the run ended).
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

-- | The route set is an index, the session key is truth: a stale member is repaired lazily under the plate lock (not re-entrant, so
-- no caller may hold it), and the SREM is skipped if the plate flapped back onto this route, so an A->B->A move isn't undone.
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

-- | A shared-cab itinerary is shown only while a cab is running its route. TODO: also require the cab not to have passed the board stop.
filterCabLegs :: (Redis.HedisFlow m r, MonadFlow m) => [MultiModalTypes.MultiModalLeg] -> m (Maybe [MultiModalTypes.MultiModalLeg])
filterCabLegs legs = do
  cabsComing <- allM hasCabComing (filter isSharedCabLeg legs)
  pure $ if cabsComing then Just (filter (not . isSharedCabLeg) legs) else Nothing
  where
    isSharedCabLeg leg = maybe False isSharedCabAgency (leg.agency >>= (.gtfsId))
    hasCabComing leg =
      maybe (return False) (fmap (not . null) . activeSessionsOnRoute . gtfsIdtoDomainCode) $
        listToMaybe leg.routeDetails >>= (.gtfsId)

liftSession :: MonadFlow m => Either SharedCabSessionError a -> m a
liftSession = either throwError pure

saveSession :: (Redis.HedisFlow m r, MonadFlow m) => Maybe Session -> Session -> m Session
saveSession old new = shared $ do
  Redis.setExp (sessionKey new.vehicleNumber) new sessionTtlSec
  let moves = routeSetMoves old new
  forM_ moves.removeFrom $ \route -> void $ Redis.srem (routeKey route) [new.vehicleNumber]
  forM_ moves.addTo $ \route -> Redis.sAddExp (routeKey route) [new.vehicleNumber] sessionTtlSec
  pure new

closeLiveTrip :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Text -> DVT.VehicleTripEndReason -> UTCTime -> m ()
closeLiveTrip plate reason now =
  QVT.findActiveByVehicleNumber plate
    >>= traverse_ (\trip -> QVT.closeTrip (closedTripStatus reason) (Just reason) (Just now) trip.id)

switchTo :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => DVT.VehicleTripEndReason -> Text -> Session -> m Session
switchTo reason newRoute s = do
  now <- getCurrentTime
  tripId <- generateGUID
  let s' = switchRoute newRoute tripId s
  withAttach (Just s) s' (replaceLiveTrip reason now (Just s) s') <* Events.forSession (Events.RouteChanged s.routeCode) s'

-- | The live-trip index allows one ACTIVE/PAUSED row per plate, so the old run closes first; if creating or saving then fails, it is reopened.
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

stranded :: [(b, Either e ())] -> [b]
stranded results = [b | (b, Left _) <- results]

-- | Every end path. Each rider drop is caught on its own: one lock timeout must not strand the others or stop the end itself.
finish :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => DVT.VehicleTripEndReason -> Session -> m Session
finish reason s = do
  onBoard <- ridersOnBoard s.vehicleNumber
  results <- forM onBoard $ \b -> (b,) <$> withTryCatch "sharedCab:dropOnFinish" (Booking.markDropped (endDropBy reason) b)
  forM_ (stranded results) $ \b -> logError $ "sharedCab: rider still on board after the session ended, booking=" <> b.id.getId <> " plate=" <> s.vehicleNumber
  now <- getCurrentTime
  -- ENDED first, since the tick claims onto ACTIVE sessions only: a crash between the steps never leaves a phantom ACTIVE cab.
  ended <- saveSession (Just s) (endSession s)
  closeLiveTrip s.vehicleNumber reason now
  clearCabDriverInfo s.vehicleNumber
  detach s
  ended <$ Events.forSession (Events.Ended (show reason)) s

-- | Open a session, or change route if this driver already has one. A change with riders on board needs a mode.
selectRoute :: Maybe SelectRouteMode -> OpenSessionReq -> Environment.Flow (Either [DFRFSTicketBooking.FRFSTicketBooking] Session)
selectRoute mode req = withPlateLock plate $ do
  enabled <- sharedCabEnabled req.merchantOperatingCityId
  unless enabled $ throwError (InvalidRequest "Shared cab is not enabled in this city")
  prior <- readSession plate
  liftSession (planSelect req.driverId req.routeCode prior) >>= \case
    KeepRoute s -> pure (Right s)
    ChangeRoute s -> do
      onBoard <- ridersOnBoard plate
      case mode of
        _ | null onBoard -> Right <$> switchTo DVT.ROUTE_CHANGED req.routeCode s
        -- TODO: re-drop forced riders at the nearest common stop and notify them.
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
      -- Seeded here rather than in `finish` (which runs from the expiry job with no scheduler constraint); a failure must not fail the open.
      withTryCatch "sharedCabSeedSweep" (ensureDegradedSweep s.merchantId s.merchantOperatingCityId) >>= \case
        Left e -> logError $ "sharedCab: couldn't seed the degraded sweep for " <> plate <> ": " <> show e
        Right () -> pure ()
      Events.forSession Events.SessionStarted opened
      pure (Right opened)
  where
    plate = canonicalisePlate req.vehicleNumber

-- | Applies an `afterLastDrop` route change once no rider is left; True when it switched (the caller then releases the old route's allocations, outside this lock).
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

endRoute :: Text -> Text -> Bool -> EndRouteAction -> Environment.Flow Session
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

expire :: (LtsFlow m r c, Events.EventFlow m r, MonadMask m) => Text -> m Session
expire rawPlate =
  withPlateLock plate $
    readSession plate >>= \case
      Just s | s.status /= ENDED -> finish DVT.SESSION_TIMEOUT s
      _ -> throwError SessionNotFound
  where
    plate = canonicalisePlate rawPlate

-- | System-initiated: no driver check. Mirrored to the trip row so flush recovery restores PAUSED.
pause :: (Events.EventFlow m r, MonadMask m) => Text -> PauseReason -> m Session
pause rawPlate reason = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ maybe (Left SessionNotFound) (pauseSession reason) prior
  QVT.updateStatus DVT.PAUSED s.vehicleTripId
  saveSession prior s <* Events.forSession (Events.Paused (show reason)) s
  where
    plate = canonicalisePlate rawPlate

resume :: Text -> Text -> Environment.Flow Session
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

setWalkupCount :: Text -> Text -> Int -> Int -> Environment.Flow Session
setWalkupCount driver rawPlate expectedVersion count = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior
  s' <- liftSession $ setWalkup expectedVersion count s
  saveWalkups DriverCounted prior s s'
  where
    plate = canonicalisePlate rawPlate

-- | Walk-ups fill every seat the boarded riders don't hold, under the plate lock BEFORE the unboarded allocations are released, so no claim lands in between.
markCabFull :: Text -> Text -> Environment.Flow Session
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

saveWalkups :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => WalkupSource -> Maybe Session -> Session -> Session -> m Session
saveWalkups source prior s s' = do
  let added = walkupsToCount source s s'
  when (added > 0) $
    QVT.findById s.vehicleTripId
      >>= traverse_ (\trip -> QVT.updateOfflineBoardings (trip.offlineBoardings + added) trip.id)
  saveSession prior s'

-- | End a rider `finish` couldn't drop. Plate lock serialises with `finish` and re-open; plate -> booking lock order.
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
