-- | A confirmed shared-cab booking is allocated now, not at the next tick. Both confirm paths end in
-- Domain.Action.Beckn.FRFS.OnConfirm.onConfirm, which calls this.
module SharedLogic.SharedCab.ConfirmHook
  ( SharedCabConfirmFlow,
    confirmHook,
    onBookingCreated,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.JourneyLeg as DJL
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.SharedCab.Allocation (triggerSharedCabAllocation)
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.LegState (sharedCabLegMode)
import SharedLogic.SharedCab.Schedule (ensureAllocationTick)
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg

type SharedCabConfirmFlow m r =
  ( JobCreator r m,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasKafkaProducer r,
    MonadMask m,
    Metrics.CoreMetrics m,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

-- | The seed also revives a dead tick chain. Non-blocking (the trigger forks) and takes no plate or booking lock.
confirmHook :: SharedCabConfirmFlow m r => DFTB.FRFSTicketBooking -> m ()
confirmHook booking =
  when (isSharedCabBooking booking) $
    -- the confirm is committed by now: a Redis or job-table failure here only means the next tick / session open seeds the chain
    withTryCatch "sharedCab:confirmHook" (ensureAllocationTick booking.merchantId booking.merchantOperatingCityId >> triggerSharedCabAllocation booking.merchantOperatingCityId)
      >>= either (\e -> logError $ "shared-cab confirm hook failed for booking " <> booking.id.getId <> ": " <> show e) pure

onBookingCreated :: Events.EventFlow m r => DFTB.FRFSTicketBooking -> Maybe DJL.JourneyLeg -> m (Maybe DJL.JourneyLeg)
onBookingCreated booking mbJourneyLeg = do
  when (booking.serviceTierType == Just SHARED_CAB) $ Events.forBooking Events.BookingCreated booking
  forM mbJourneyLeg $ \leg ->
    let legMode = sharedCabLegMode booking.serviceTierType leg.mode
     in if legMode == leg.mode
          then pure leg
          else do
            QJourneyLeg.updateMode legMode leg.id
            legs <- QJourneyLeg.getJourneyLegs leg.journeyId
            QJourneyExtra.updateModes [if l.id == leg.id then legMode else l.mode | l <- sortOn (.sequenceNumber) legs] leg.journeyId
            pure leg {DJL.mode = legMode}
