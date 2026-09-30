-- | R67: a confirmed shared-cab booking is allocated now, not at the next tick. Both confirm paths end in
-- Domain.Action.Beckn.FRFS.OnConfirm.onConfirm (the ONDC callback and the direct / sync ExternalBPP flow), which calls this.
module SharedLogic.SharedCab.ConfirmHook
  ( SharedCabConfirmFlow,
    onSharedCabConfirmed,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.FRFSTicketBooking as DFTB
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.SharedCab.Allocation (triggerSharedCabAllocation)
import SharedLogic.SharedCab.AllocationSchedule (ensureAllocationTick)
import SharedLogic.SharedCab.Booking (isSharedCabBooking)

type SharedCabConfirmFlow m r =
  ( JobCreator r m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasKafkaProducer r,
    MonadMask m,
    Metrics.CoreMetrics m,
    ServiceFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

-- | The seed also revives a dead tick chain. Non-blocking (the trigger forks) and takes no plate or booking lock.
onSharedCabConfirmed :: SharedCabConfirmFlow m r => DFTB.FRFSTicketBooking -> m ()
onSharedCabConfirmed booking =
  when (isSharedCabBooking booking) $
    -- the confirm is committed by now: a Redis or job-table failure here only means the next tick / session open seeds the chain
    withTryCatch "sharedCab:confirmHook" (ensureAllocationTick booking.merchantId booking.merchantOperatingCityId >> triggerSharedCabAllocation booking.merchantOperatingCityId)
      >>= either (\e -> logError $ "shared-cab confirm hook failed for booking " <> booking.id.getId <> ": " <> show e) pure
