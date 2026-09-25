-- | One SharedCabSessionExpiry job chain per city, with no ops seeding: every session open re-seeds the chain once its
-- guard lapses, and a duplicate chain dies at its next run because another chain already claimed the tick.
-- Keys are unprefixed because the API and the scheduler use different key prefixes.
module SharedLogic.SharedCab.ExpirySchedule
  ( ensureExpiryJob,
    claimTick,
    scheduleNextExpiry,
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

tickSec :: Int
tickSec = 60

guardKey :: Id DMOC.MerchantOperatingCity -> Text
guardKey mocId = "sharedcab:expiryJob:" <> mocId.getId

tickKey :: Id DMOC.MerchantOperatingCity -> Text
tickKey mocId = "sharedcab:expiryTick:" <> mocId.getId

setNx :: (Redis.HedisFlow m r, MonadFlow m) => Text -> Int -> m Bool
setNx key ttl = shared $ Redis.setNxExpire key ttl ()

ensureExpiryJob :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
ensureExpiryJob merchantId mocId =
  whenM (setNx (guardKey mocId) (2 * tickSec)) $ createNext merchantId mocId

-- | False when another chain already ran within this tick: the caller stops without rescheduling.
claimTick :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Bool
claimTick mocId = setNx (tickKey mocId) (tickSec - 5)

scheduleNextExpiry :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
scheduleNextExpiry merchantId mocId = do
  shared $ Redis.setExp (guardKey mocId) () (2 * tickSec)
  createNext merchantId mocId

createNext :: JobCreator r m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
createNext merchantId mocId =
  createJobIn @_ @'SharedCabSessionExpiry (Just merchantId) (Just mocId) (intToNominalDiffTime tickSec) $
    SharedCabSessionExpiryJobData {merchantId, merchantOperatingCityId = mocId}
