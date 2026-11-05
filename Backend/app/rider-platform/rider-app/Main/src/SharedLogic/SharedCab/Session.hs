-- | Rider-app's copy of each live shared cab. Driver-app owns the run (vehicle_trip, route, status, the LTS attach) and sends every
-- change through /internal/sharedCab/cab/sync ('SharedLogic.SharedCab.CabSync' writes the copy); rider-app owns the seats, so
-- walk-ups and cab full are written here, under the same plate lock that every seat claim takes.
module SharedLogic.SharedCab.Session
  ( stranded,
    getSession,
    readSession,
    saveSession,
    CabDriverInfo (..),
    setCabDriverInfo,
    clearCabDriverInfo,
    readCabDriverInfo,
    withPlateLock,
    activeSessionsOnRoute,
    cityLiveSessions,
    setWalkupCount,
    markCabFull,
    dropStrandedRider,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Database.Redis as RawRedis
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DBookingStatus
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import SharedLogic.SharedCab.Keys (cityCabsKey, driverInfoKey, legacyRouteKey, plateLockKey, routeKey, sessionKey)
import SharedLogic.SharedCab.Lock (withOwnedLock)
import qualified SharedLogic.SharedCab.Seats as Seats
import SharedLogic.SharedCab.SessionState
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QBooking

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

-- | Rider-app's plate lock: every seat write (claim, boarding, walk-ups) and every copy write. Never held across a call to
-- driver-app, which takes its own plate lock and may be waiting on rider-app.
withPlateLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Text -> m a -> m a
withPlateLock plate = withOwnedLock "plateLock" (plateLockKey plate) lockTtlSec 25000

readSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
readSession = shared . Redis.safeGet . sessionKey

-- | The copy only: a Redis flush leaves the cab out until driver-app's next sync (its session poll, every few seconds).
getSession :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m (Maybe Session)
getSession = readSession

-- | The route set is an index, the session key is truth: a stale member is repaired lazily under the plate lock (not re-entrant, so
-- no caller may hold it), and the SREM is skipped if the plate flapped back onto this route, so an A->B->A move isn't undone.
activeSessionsOnRoute :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DMOC.MerchantOperatingCity -> Text -> m [Session]
activeSessionsOnRoute cityId route = do
  plates <- shared $ Redis.sMembers (routeKey cityId route)
  legacyPlates <- shared $ Redis.sMembers (legacyRouteKey route)
  catMaybes <$> mapM keepOrRepair (nub (plates <> legacyPlates))
  where
    keepOrRepair plate =
      routeMember route <$> readSessionStrict plate >>= \case
        -- the legacy set is shared by every city: another city's cab on the same route code is live, not stale
        LiveMember s -> pure $ if s.merchantOperatingCityId == cityId then Just s else Nothing
        StaleMember -> repair plate >> pure Nothing
        UnknownMember -> Nothing <$ logWarning ("sharedCab: session of " <> plate <> " on " <> route <> " unreadable, route-set repair skipped")
    repair plate =
      withTryCatch "sharedCab:repairRouteSet" (withPlateLock plate (reread plate))
        >>= either (\e -> logWarning $ "sharedCab: route-set repair of " <> plate <> " on " <> route <> " skipped: " <> show e) pure
    reread plate =
      routeMember route <$> readSessionStrict plate >>= \case
        StaleMember -> void $ shared $ Redis.srem (routeKey cityId route) [plate] >> Redis.srem (legacyRouteKey route) [plate]
        _ -> pure ()

-- | The city's live cabs (ACTIVE or PAUSED), for the expiry and stop-progress passes. A member whose copy is gone or ended is
-- skipped and dropped from the set.
cityLiveSessions :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m [Session]
cityLiveSessions cityId = do
  plates <- shared $ Redis.sMembers (cityCabsKey cityId)
  sessions <- if null plates then pure [] else shared (Redis.mGetCluster (map sessionKey plates))
  let live = [s | s <- sessions, s.status /= ENDED, s.merchantOperatingCityId == cityId]
      gone = filter (`notElem` map (.vehicleNumber) live) plates
  unless (null gone) $ void $ shared $ Redis.srem (cityCabsKey cityId) gone
  pure live

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
  if new.status == ENDED
    then void $ Redis.srem (cityCabsKey new.merchantOperatingCityId) [new.vehicleNumber]
    else Redis.sAddExp (cityCabsKey new.merchantOperatingCityId) [new.vehicleNumber] sessionTtlSec
  pure new

stranded :: [(b, Either e ())] -> [b]
stranded results = [b | (b, Left _) <- results]

-- | The walk-ups the driver counted, and how many were added: driver-app puts those on the trip's offline boardings.
setWalkupCount :: Text -> Text -> Int -> Int -> Environment.Flow (Session, Int)
setWalkupCount driver plate expectedVersion count = withPlateLock plate $ do
  prior <- readSession plate
  s <- liftSession $ ownedSession driver prior
  requireSharedCabEnabled s.merchantOperatingCityId
  s' <- liftSession $ setWalkup expectedVersion count s
  held <- Seats.setWalkups s count
  let counted = s' {walkupCount = held}
  (,walkupsToCount DriverCounted s counted) <$> saveSession prior counted

-- | Cab full: `releaseUnboarded` (the unboarded allocations going) runs under the plate lock, then walk-ups take every seat left,
-- so no claim lands in between. Its result comes back for the caller to send the releases' effects outside the lock.
markCabFull :: Text -> Text -> Environment.Flow a -> Environment.Flow (Session, a)
markCabFull driver plate releaseUnboarded =
  withPlateLock plate $ do
    prior <- readSession plate
    s <- liftSession $ ownedSession driver prior
    released <- releaseUnboarded
    held <- Seats.setWalkups s s.capacity
    (,released) <$> saveSession prior (fillCab held s)

-- | End a rider the cab's end couldn't drop. Plate lock serialises with the end and a re-open; plate -> booking lock order. A plate
-- with no copy at all (a flush) is left alone: the copy comes back on the driver's next sync.
dropStrandedRider :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m, MonadMask m) => DFRFSTicketBooking.FRFSTicketBooking -> m Bool
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
    isStranded plate b
      | b.status /= DBookingStatus.CONFIRMED = pure False
      | otherwise = do
        mbSession <- readSession plate
        statuses <- map (.status) <$> QTicket.findAllByTicketBookingId b.id
        pure $ shouldDropOnDeadSession ((.status) <$> mbSession) (isNothing mbSession) statuses
