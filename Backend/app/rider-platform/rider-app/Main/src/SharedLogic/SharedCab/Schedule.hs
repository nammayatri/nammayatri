{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The shared-cab per-city job chains and their city leases. A chain's guard key holds the id of the one live chain; every tick
-- carries its chain's id and stops, without rescheduling, once the guard names another. A seed after a lapse makes a new id, so the
-- old chain dies at its next tick instead of doubling.
module SharedLogic.SharedCab.Schedule
  ( -- * Allocation tick
    ensureAllocationTick,
    ChainVerdict (..),
    chainVerdict,
    ownAllocationChain,
    claimTickRun,
    scheduleNextTick,
    claimedTick,
    withCityTickLease,
    ensureExpiryJob,
    WatchPlan (..),
    watchPlan,
    ownExpiryChain,
    claimExpiryRun,
    scheduleNextExpiry,
    sweepTickSec,
    scanWindowStart,
    ensureDegradedSweep,
    ownSweepChain,
    claimSweepRun,
    claimRefundRetryRun,
    withSweepLease,
    scheduleNextSweep,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Booking (shared)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import SharedLogic.SharedCab.Keys
import Storage.Beam.SchedulerJob ()

data Chain m = Chain Text Int (Text -> m ())

chain ::
  forall (e :: RiderJobType) m r.
  (JobFlow RiderJobType e, JobCreator r m) =>
  Text ->
  Int ->
  Int ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  (Text -> JobContent e) ->
  Chain m
chain guardKey guardTtlSec intervalSec merchantId mocId jobData =
  Chain guardKey guardTtlSec $ createJobIn @_ @e (Just merchantId) (Just mocId) (intToNominalDiffTime intervalSec) . jobData

setNx :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> m Bool
setNx key ttl = shared $ Redis.setNxExpire key ttl ()

ensureChain :: (Redis.HedisFlow m r, MonadFlow m) => Chain m -> m ()
ensureChain (Chain guardKey guardTtlSec enqueue) = do
  chainId <- generateGUIDText
  whenM (shared $ Redis.setNxExpire guardKey guardTtlSec chainId) (enqueue chainId)

scheduleNext :: (Redis.HedisFlow m r, MonadFlow m) => Chain m -> Text -> m ()
scheduleNext (Chain guardKey guardTtlSec enqueue) chainId = do
  shared $ Redis.setExp guardKey chainId guardTtlSec
  enqueue chainId

-- | What a tick does with the guard it finds: its own chain's id (go on), none (the guard lapsed: take it back, unless a seed
-- wins the race), or another chain's id (stop).
data ChainVerdict = Mine | Adopt | Foreign
  deriving (Eq, Show)

chainVerdict :: Maybe Text -> Maybe Text -> ChainVerdict
chainVerdict Nothing _ = Adopt
chainVerdict (Just live) mine = if mine == Just live then Mine else Foreign

-- | Runs a tick's body only if its chain still holds the guard; the body gets the chain id to hand to its next tick. A tick of
-- a chain that a lapse and a re-seed replaced finds the new id and ends here.
ownChain :: (Redis.HedisFlow m r, MonadFlow m) => Chain m -> Maybe Text -> (Text -> m ()) -> m ()
ownChain chain'@(Chain guardKey guardTtlSec _) mbChainId body = do
  fresh <- generateGUIDText
  let chainId = fromMaybe fresh mbChainId
  -- a guard that cannot be read leaves the chain's fate unknown: keep it going, a duplicate dies at its next tick
  withTryCatch "sharedCab:ownChain" (shared $ Redis.safeGet guardKey) >>= \case
    Left err -> logError ("sharedCab chain " <> guardKey <> ": guard unreadable, keeping the chain: " <> show err) >> scheduleNext chain' chainId
    Right live -> case chainVerdict live mbChainId of
      Mine -> body chainId
      Adopt -> whenM (shared $ Redis.setNxExpire guardKey guardTtlSec chainId) (body chainId)
      Foreign -> logInfo $ "sharedCab chain " <> guardKey <> ": guard names another chain; this one ends"

withCityLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Text -> Int -> m () -> m ()
withCityLease key ttl action =
  whenM (shared $ Redis.tryLockRedis key ttl) $ action `finally` shared (Redis.unlockRedis key)

-- | A lease outlives its holder by this many ticks at most; a job guard outlives a missed tick by this many.
leaseTickMultiple, guardTickMultiple, scanWindowHorizons :: Int
leaseTickMultiple = 10
guardTickMultiple = 2
scanWindowHorizons = 3

tickSecOf :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Int
tickSecOf mocId = (.tickSec) <$> Config.getTunables mocId

allocChain :: JobCreator r m => Int -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Chain m
allocChain tickSec merchantId mocId =
  chain @'SharedCabAllocationTick (allocJobGuardKey mocId) (leaseTickMultiple * tickSec) tickSec merchantId mocId $
    \chainId -> SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId = mocId, chainId = Just chainId}

ensureAllocationTick :: (JobCreator r m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureAllocationTick merchantId mocId =
  whenM (sharedCabEnabled mocId) $ do
    tickSec <- tickSecOf mocId
    ensureChain (allocChain tickSec merchantId mocId)

claimTickRun :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
claimTickRun mocId = do
  tickSec <- tickSecOf mocId
  setNx (allocRunKey mocId) (max 1 (tickSec - 1))

scheduleNextTick :: (JobCreator r m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> m ()
scheduleNextTick merchantId mocId chainId = do
  tickSec <- tickSecOf mocId
  scheduleNext (allocChain tickSec merchantId mocId) chainId

ownAllocationChain :: (JobCreator r m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> (Text -> m ()) -> m ()
ownAllocationChain merchantId mocId mbChainId body = do
  tickSec <- tickSecOf mocId
  ownChain (allocChain tickSec merchantId mocId) mbChainId body

-- | A tick's run claim and body, then the next tick even when either throws, or one Redis or DB hiccup ends the city's chain for
-- good. A tick whose run was already claimed (the producer dispatched its job twice) ends without scheduling.
claimedTick :: (MonadMask m, MonadIO m) => m Bool -> m () -> m () -> m ()
claimedTick claim body next = do
  dropped <- liftIO (newIORef False)
  (claim >>= \claimed -> if claimed then body else liftIO (writeIORef dropped True))
    `finally` (liftIO (readIORef dropped) >>= \isDropped -> unless isDropped next)

-- | One tick per city; the lease TTL only bounds a crashed holder.
withCityTickLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m () -> m ()
withCityTickLease cityId action = do
  tickSec <- tickSecOf cityId
  withCityLease (allocLeaseKey cityId) (leaseTickMultiple * tickSec) action

expiryTickSec :: Int
expiryTickSec = 60

expiryChain :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Chain m
expiryChain merchantId mocId =
  chain @'SharedCabSessionExpiry (expiryJobGuardKey mocId) (guardTickMultiple * expiryTickSec) expiryTickSec merchantId mocId $
    \chainId -> SharedCabSessionExpiryJobData {merchantId, merchantOperatingCityId = mocId, chainId = Just chainId}

-- | The expiry tick doubles as the other chains' watchdog, and the sweep tick as the expiry chain's: a job that was lost or wedged
-- ends its chain silently, and a chain is otherwise re-seeded only by new demand. Each seed is a set-if-absent on the chain's guard,
-- so watching a live chain is a no-op and cannot double it.
data WatchPlan = WatchPlan {seedAllocation :: Bool, seedSweep :: Bool}
  deriving (Eq, Show)

-- | An allocation chain is wanted while the city has a live booking, a sweep chain while cabs are on shift.
watchPlan :: Int -> Int -> WatchPlan
watchPlan liveBookings liveTrips = WatchPlan {seedAllocation = liveBookings > 0, seedSweep = liveTrips > 0}

ensureExpiryJob :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureExpiryJob merchantId mocId = ensureChain (expiryChain merchantId mocId)

claimExpiryRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimExpiryRun mocId = setNx (expiryTickKey mocId) (expiryTickSec - 5)

scheduleNextExpiry :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> m ()
scheduleNextExpiry merchantId mocId = scheduleNext (expiryChain merchantId mocId)

ownExpiryChain :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> (Text -> m ()) -> m ()
ownExpiryChain merchantId mocId = ownChain (expiryChain merchantId mocId)

-- | Sweep cadence, far below the degraded horizon: a ride whose rider never polls ends at most one tick after its marker dies.
sweepTickSec :: Int
sweepTickSec = 120

-- | The scan's updatedAt front bound is 3x the degraded horizon h: a ride degraded at t is killable from t+h and scanned until t+3h,
-- so a sweep-down gap shorter than 2h loses nothing.
scanWindowStart :: Int -> UTCTime -> UTCTime
scanWindowStart horizonSec = addUTCTime (negate (intToNominalDiffTime (scanWindowHorizons * horizonSec)))

sweepChain :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Chain m
sweepChain merchantId mocId =
  chain @'SharedCabDegradedSweep (sweepJobGuardKey mocId) (guardTickMultiple * sweepTickSec) sweepTickSec merchantId mocId $
    \chainId -> SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId = mocId, chainId = Just chainId}

-- | Armed by a degraded boarding, a session open and a failed refund start. An empty scan window lets the chain lapse; the next armer re-seeds it.
ensureDegradedSweep :: (JobCreator r m, Redis.HedisFlow m r, MonadFlow m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureDegradedSweep merchantId mocId =
  ensureChain (sweepChain merchantId mocId)

claimSweepRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimSweepRun mocId = setNx (sweepRunKey mocId) (sweepTickSec - 5)

claimRefundRetryRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimRefundRetryRun mocId = setNx (refundRetryRunKey mocId) (sweepTickSec - 5)

-- | One sweep body at a time per city. The lease carries an owner token checked on unlock, so a sweep outliving its TTL cannot free a later holder's lease.
withSweepLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DMOC.MerchantOperatingCity -> m a -> m (Maybe a)
withSweepLease mocId action = do
  owner <- generateGUIDText
  let key = Redis.buildLockResourceName (sweepLeaseKey mocId)
  acquired <- shared $ Redis.setNxExpire key (leaseTickMultiple * sweepTickSec) owner
  if not acquired
    then pure Nothing
    else
      (Just <$> action) `finally` shared do
        current <- Redis.safeGet key
        when (current == Just owner) $ Redis.unlockRedis (sweepLeaseKey mocId)

scheduleNextSweep :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> m ()
scheduleNextSweep merchantId mocId = scheduleNext (sweepChain merchantId mocId)

ownSweepChain :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> (Text -> m ()) -> m ()
ownSweepChain merchantId mocId = ownChain (sweepChain merchantId mocId)
