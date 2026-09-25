module SharedLogic.SharedCab.Session
  ( selectRoute,
    changeRoute,
    applyQueuedRoute,
    endRoute,
    pause,
    resume,
    getSession,
    activeSessionsOnRoute,
    setWalkupCount,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.VehicleTrip as DVT
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (ridersOnBoard)
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error (SharedCabSessionError (..))

sessionKey :: Text -> Text
sessionKey plate = "sharedcab:session:" <> plate

routeKey :: Text -> Text
routeKey routeCode = "sharedcab:route:" <> routeCode

lockKey :: Text -> Text
lockKey plate = "sharedcab:lock:" <> plate

-- rider_config.maxSessionHours default; read from config once that field exists.
sessionTtlSec :: Int
sessionTtlSec = 16 * 60 * 60

lockTtlSec :: Int
lockTtlSec = 10

withPlateLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Text -> m a -> m a
withPlateLock plate =
  Redis.withMasterRedis . Redis.withWaitAndLockRedis (lockKey plate) lockTtlSec 10000

readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = Redis.safeGet . sessionKey

getSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
getSession = Redis.withMasterRedis . readSession . canonicalisePlate

-- | Each member is re-read: a route set can outlive a session that has since paused or ended.
activeSessionsOnRoute :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m [Session]
activeSessionsOnRoute route = Redis.withMasterRedis $ do
  plates <- Redis.sMembers (routeKey route)
  filter ((== ACTIVE) . (.status)) . catMaybes <$> mapM readSession plates

liftSession :: MonadFlow m => Either SharedCabSessionError a -> m a
liftSession = either throwError pure

saveSession :: (Redis.HedisFlow m r, MonadFlow m) => Maybe Session -> Session -> m Session
saveSession old new = do
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

switchTo :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => DVT.VehicleTripEndReason -> Text -> Session -> m Session
switchTo reason newRoute s = do
  now <- getCurrentTime
  tripId <- generateGUID
  let s' = switchRoute newRoute tripId s
  closeLiveTrip s.vehicleNumber reason now
  QVT.create (tripFor s' now)
  saveSession (Just s) s'

-- | Open a session, or change route if this driver already has one (idempotent for the same route).
-- A change with riders on board (`04` §4) needs a mode: Left lists them when there is none.
selectRoute ::
  (ServiceFlow m r, MonadFlow m, MonadMask m) =>
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
      closeLiveTrip plate DVT.SESSION_TIMEOUT now
      QVT.create (tripFor s now)
      Right <$> saveSession prior s
  where
    plate = canonicalisePlate req.vehicleNumber

changeRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Text -> m Session
changeRoute driver rawPlate newRoute = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  if s.routeCode == newRoute then pure s else switchTo DVT.ROUTE_CHANGED newRoute s
  where
    plate = canonicalisePlate rawPlate

-- | Call after each drop: applies an `afterLastDrop` route change once no rider is left on board.
applyQueuedRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> m ()
applyQueuedRoute rawPlate = withPlateLock plate $ do
  mbSession <- readSession plate
  whenJust mbSession $ \s -> whenJust s.queuedRouteCode $ \queued ->
    when (s.status /= ENDED) $ do
      onBoard <- ridersOnBoard plate
      when (null onBoard) $ void $ switchTo DVT.ROUTE_CHANGED queued s
  where
    plate = canonicalisePlate rawPlate

-- | Refused while riders are on board unless `forced` (`04` §7: forced riders fall to the degraded timeout).
-- Forced riders are told: a return trip is a route change, an end asks them to confirm their drop.
endRoute :: (ServiceFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Bool -> EndRouteAction -> m Session
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
      now <- getCurrentTime
      closeLiveTrip plate (endActionReason action) now
      s' <- saveSession (Just s) (endSession s)
      s' <$ mapM_ Notify.notifyDropConfirm onBoard
  where
    plate = canonicalisePlate rawPlate

-- | System-initiated (tick, offline toggle): no driver check.
pause :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> PauseReason -> m Session
pause rawPlate reason = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ maybe (Left SessionNotFound) (pauseSession reason) prior
  saveSession prior s
  where
    plate = canonicalisePlate rawPlate

resume :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> m Session
resume driver rawPlate = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior >>= resumeSession
  saveSession prior s
  where
    plate = canonicalisePlate rawPlate

-- | Compare-and-set on `version`; each walk-up added is also counted on the trip row (offlineBoardings).
setWalkupCount :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Int -> Int -> m Session
setWalkupCount driver rawPlate expectedVersion count = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior
  s' <- liftSession $ setWalkup expectedVersion count s
  let added = count - s.walkupCount
  when (added > 0) $
    QVT.findById s.vehicleTripId
      >>= traverse_ (\trip -> QVT.updateOfflineBoardings (trip.offlineBoardings + added) trip.id)
  saveSession prior s'
  where
    plate = canonicalisePlate rawPlate
