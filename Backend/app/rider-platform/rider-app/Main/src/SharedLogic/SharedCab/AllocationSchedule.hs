-- | One SharedCabAllocationTick job chain per city, seeded on session open (a FINDING booking can only be
-- allocated to an ACTIVE session, so a city with no session needs no tick). A seed re-creates the chain once
-- its guard lapses; a duplicate chain dies at its next run because another chain already claimed that tick.
-- Keys are unprefixed (cross-app) because the API and the scheduler use different key prefixes.
module SharedLogic.SharedCab.AllocationSchedule
  ( ensureAllocationTick,
    claimTickRun,
    scheduleNextTick,
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
import SharedLogic.SharedCab.Allocation (sharedCabAllocationEnabled)
import SharedLogic.SharedCab.Allocation.Types (defaultAllocationConfig)
import SharedLogic.SharedCab.Booking (shared)
import Storage.Beam.SchedulerJob ()

tickSec :: Int
tickSec = defaultAllocationConfig.tickSec -- //TODO(05 §7): rider_config bind

-- The scheduler polls every few seconds, so the chain's guard spans many ticks.
guardTtlSec :: Int
guardTtlSec = 10 * tickSec

guardKey :: Id DMOC.MerchantOperatingCity -> Text
guardKey mocId = "sharedcab:allocJob:" <> mocId.getId

runKey :: Id DMOC.MerchantOperatingCity -> Text
runKey mocId = "sharedcab:allocRun:" <> mocId.getId

setNx :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> m Bool
setNx key ttl = shared $ Redis.setNxExpire key ttl ()

-- | Idempotent; no job while the engine is gated off.
ensureAllocationTick :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureAllocationTick merchantId mocId =
  when sharedCabAllocationEnabled $
    whenM (setNx (guardKey mocId) guardTtlSec) $ createNext merchantId mocId

-- | False when another chain already ran this tick: the caller stops without rescheduling.
claimTickRun :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimTickRun mocId = setNx (runKey mocId) (max 1 (tickSec - 1))

scheduleNextTick :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextTick merchantId mocId = do
  shared $ Redis.setExp (guardKey mocId) () guardTtlSec
  createNext merchantId mocId

createNext :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
createNext merchantId mocId =
  createJobIn @_ @'SharedCabAllocationTick (Just merchantId) (Just mocId) (intToNominalDiffTime tickSec) $
    SharedCabAllocationTickJobData {merchantId, merchantOperatingCityId = mocId}
