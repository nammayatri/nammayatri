module SharedLogic.SharedCab.Session
  ( selectRoute,
    changeRoute,
    endRoute,
    pause,
    resume,
    getSession,
    activeSessionsOnRoute,
    setWalkupCount,
  )
where

import qualified Domain.Types.VehicleTrip as DVT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
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
selectRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => OpenSessionReq -> m Session
selectRoute req = withPlateLock plate $ do
  prior <- readSession plate
  liftSession (planSelect req.driverId req.routeCode prior) >>= \case
    KeepRoute s -> pure s
    ChangeRoute s -> switchTo DVT.ROUTE_CHANGED req.routeCode s
    OpenSession -> do
      now <- getCurrentTime
      tripId <- generateGUID
      let s = newSession req tripId now prior
      closeLiveTrip plate DVT.SESSION_TIMEOUT now
      QVT.create (tripFor s now)
      saveSession prior s
  where
    plate = canonicalisePlate req.vehicleNumber

changeRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> Text -> m Session
changeRoute driver rawPlate newRoute = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  if s.routeCode == newRoute then pure s else switchTo DVT.ROUTE_CHANGED newRoute s
  where
    plate = canonicalisePlate rawPlate

endRoute :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => Text -> Text -> EndRouteAction -> m Session
endRoute driver rawPlate action = withPlateLock plate $ do
  s <- readSession plate >>= liftSession . ownedSession driver
  case action of
    StartReturn -> liftSession (returnRouteOf s.routeCode) >>= \returnRoute -> switchTo DVT.RETURN returnRoute s
    _ -> do
      now <- getCurrentTime
      closeLiveTrip plate (endActionReason action) now
      saveSession (Just s) (endSession s)
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
