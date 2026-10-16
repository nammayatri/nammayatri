{-# LANGUAGE AllowAmbiguousTypes #-}

-- | The shared-cab per-city job chains and their city leases. A seed re-creates a chain once its guard lapses; a duplicate chain
-- dies at its next run because that tick's run key is already claimed.
module SharedLogic.SharedCab.Schedule
  ( -- * Allocation tick
    ensureAllocationTick,
    claimTickRun,
    scheduleNextTick,
    thenReschedule,
    withCityTickLease,
    ensureExpiryJob,
    claimExpiryRun,
    scheduleNextExpiry,
    sweepTickSec,
    scanWindowStart,
    ensureDegradedSweep,
    claimSweepRun,
    claimRefundRetryRun,
    withSweepLease,
    scheduleNextSweep,
  )
where

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

data Chain m = Chain Text Int (m ())

chain ::
  forall (e :: RiderJobType) m r.
  (JobFlow RiderJobType e, JobCreator r m) =>
  Text ->
  Int ->
  Int ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  JobContent e ->
  Chain m
chain guardKey guardTtlSec intervalSec merchantId mocId jobData =
  Chain guardKey guardTtlSec $ createJobIn @_ @e (Just merchantId) (Just mocId) (intToNominalDiffTime intervalSec) jobData

setNx :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> m Bool
setNx key ttl = shared $ Redis.setNxExpire key ttl ()

ensureChain :: (Redis.HedisFlow m r, MonadFlow m) => Chain m -> m ()
ensureChain (Chain guardKey guardTtlSec enqueue) = whenM (setNx guardKey guardTtlSec) enqueue

scheduleNext :: (Redis.HedisFlow m r, MonadFlow m) => Chain m -> m ()
scheduleNext (Chain guardKey guardTtlSec enqueue) = do
  shared $ Redis.setExp guardKey () guardTtlSec
  enqueue

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
    SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId = mocId}

ensureAllocationTick :: (JobCreator r m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureAllocationTick merchantId mocId =
  whenM (sharedCabEnabled mocId) $ do
    tickSec <- tickSecOf mocId
    ensureChain (allocChain tickSec merchantId mocId)

claimTickRun :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
claimTickRun mocId = do
  tickSec <- tickSecOf mocId
  setNx (allocRunKey mocId) (max 1 (tickSec - 1))

scheduleNextTick :: (JobCreator r m, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextTick merchantId mocId = do
  tickSec <- tickSecOf mocId
  scheduleNext (allocChain tickSec merchantId mocId)

thenReschedule :: MonadMask m => m () -> m () -> m ()
thenReschedule body reschedule = body `finally` reschedule

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
    SharedCabSessionExpiryJobData {merchantId, merchantOperatingCityId = mocId}

ensureExpiryJob :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureExpiryJob merchantId mocId = ensureChain (expiryChain merchantId mocId)

claimExpiryRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimExpiryRun mocId = setNx (expiryTickKey mocId) (expiryTickSec - 5)

scheduleNextExpiry :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextExpiry merchantId mocId = scheduleNext (expiryChain merchantId mocId)

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
    SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId = mocId}

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

scheduleNextSweep :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextSweep merchantId mocId = scheduleNext (sweepChain merchantId mocId)
