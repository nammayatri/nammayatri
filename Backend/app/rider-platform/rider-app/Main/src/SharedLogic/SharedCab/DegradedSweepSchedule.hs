-- | One SharedCabDegradedSweep job chain per city (M8.5): the timeout end of 05 §5 for degraded rides
-- whose rider never polls again. Seeded when a ride boards degraded (SharedLogic.SharedCab.Boarding's
-- degradedBoarding): a city with no degraded ride needs no sweep, and a fresh degrade re-seeds a chain
-- whose guard lapsed. One run per city per tick (claimSweepRun kills duplicate chains), the sweep body
-- under a city lease whose TTL outlives a sweep (a crashed holder frees it on TTL), and every run
-- re-enqueues the next in `finally`, so a partial failure never kills the chain.
-- Keys are unprefixed (cross-app master cell, Booking.shared): the API seeds, the scheduler runs.
module SharedLogic.SharedCab.DegradedSweepSchedule
  ( sharedCabDegradedSweepEnabled,
    sweepTickSec,
    scanWindowStart,
    -- redis key contract
    sweepJobGuardKey,
    sweepRunKey,
    sweepLeaseKey,
    ensureDegradedSweep,
    claimSweepRun,
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
import Storage.Beam.SchedulerJob ()

-- | HARD GATE (same pattern as Allocation.sharedCabAllocationEnabled): this chain is the first writer to
-- end rides outside the rider-poll path. Flip only after the M8.5 scenario run, as a human decision.
sharedCabDegradedSweepEnabled :: Bool
sharedCabDegradedSweepEnabled = False

-- | Sweep cadence, far below the degraded horizon (Config.degradedTimeoutSec is minutes-to-hours):
-- a ride whose rider never polls ends at most one tick after its marker dies. The scan is bounded
-- (createdAt-front 3x the horizon, keyset-paged), so a tight tick costs about nothing.
sweepTickSec :: Int
sweepTickSec = 120

-- | The candidate scan's createdAt front bound: 3x the degraded-expiry horizon h, not one horizon.
-- A ride degraded at booking creation is killable from createdAt+h (marker dead, tickets still
-- INPROGRESS) and stays in the scan until createdAt+3h, so a sweep-down gap shorter than 2h loses
-- nothing (M8.5 scan-window rule; h = Config.degradedTimeoutSec, default 60*60 in Config.defaultTunables).
scanWindowStart :: Int -> UTCTime -> UTCTime
scanWindowStart horizonSec now = addUTCTime (negate (intToNominalDiffTime (3 * horizonSec))) now

sweepJobGuardKey :: Id DMOC.MerchantOperatingCity -> Text
sweepJobGuardKey mocId = "sharedcab:degradedSweepJob:" <> mocId.getId

sweepRunKey :: Id DMOC.MerchantOperatingCity -> Text
sweepRunKey mocId = "sharedcab:degradedSweepRun:" <> mocId.getId

sweepLeaseKey :: Id DMOC.MerchantOperatingCity -> Text
sweepLeaseKey mocId = "sharedcab:degradedSweepLease:" <> mocId.getId

setNx :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> m Bool
setNx key ttl = shared $ Redis.setNxExpire key ttl ()

-- | Idempotent; no chain while the sweep is gated off.
ensureDegradedSweep :: (JobCreator r m, Redis.HedisFlow m r, MonadFlow m) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureDegradedSweep merchantId mocId =
  when sharedCabDegradedSweepEnabled $
    whenM (setNx (sweepJobGuardKey mocId) (2 * sweepTickSec)) $ createNext merchantId mocId

-- | False when another chain already ran within this tick: the caller stops without rescheduling.
claimSweepRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimSweepRun mocId = setNx (sweepRunKey mocId) (sweepTickSec - 5)

-- | One sweep body at a time per city: a shard-duplicated run that finds the lease held skips silently.
-- The TTL (10x the tick) comfortably outlives a sweep; it only bounds a crashed holder.
withSweepLease :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => Id DMOC.MerchantOperatingCity -> m () -> m ()
withSweepLease mocId action =
  whenM (shared $ Redis.tryLockRedis (sweepLeaseKey mocId) (10 * sweepTickSec)) $
    action `finally` shared (Redis.unlockRedis (sweepLeaseKey mocId))

scheduleNextSweep :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextSweep merchantId mocId = do
  shared $ Redis.setExp (sweepJobGuardKey mocId) () (2 * sweepTickSec)
  createNext merchantId mocId

createNext :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
createNext merchantId mocId =
  createJobIn @_ @'SharedCabDegradedSweep (Just merchantId) (Just mocId) (intToNominalDiffTime sweepTickSec) $
    SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId = mocId}
