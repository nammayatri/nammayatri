module SharedLogic.Scheduler.Jobs.SilentReallocationExpiry where

import qualified Domain.Types.Booking as DB
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified SharedLogic.SilentReallocation as SilentRealloc
import qualified Storage.Queries.Booking as QB
import qualified Tools.Notifications as Notify

-- | End of a silent reallocation window. If the window is still open for this booking,
-- close it and send the reallocation push that estimate-repetition held back. If a new
-- driver was assigned, the rider cancelled, or a confirm is already in flight for the
-- same search, the key is gone or a newer active booking exists and nothing is sent.
silentReallocationExpiry ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasKafkaProducer r
  ) =>
  Job 'SilentReallocationExpiry ->
  m ExecutionResult
silentReallocationExpiry Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
  mbCtx <- SilentRealloc.takeSilentReallocationForExpiry personId
  case mbCtx of
    Just ctx | ctx.bookingId == jobData.bookingId -> do
      booking <- QB.findById ctx.bookingId >>= fromMaybeM (BookingDoesNotExist ctx.bookingId.getId)
      mbNewerBooking <- QB.findByTransactionIdAndStatusWithKVAndDB booking.transactionId DB.activeBookingStatus
      case mbNewerBooking of
        Just newer -> logInfo $ "silentReallocationExpiry: newer active booking " <> newer.id.getId <> " exists, skipping reallocation push"
        Nothing -> do
          logInfo $ "silentReallocationExpiry: window ended for booking " <> ctx.bookingId.getId <> ", sending held reallocation push"
          Notify.notifyOnEstOrQuoteReallocated ctx.cancellationSource booking ctx.estimateId.getId
    _ -> logInfo $ "silentReallocationExpiry: no open window for booking " <> jobData.bookingId.getId <> ", nothing to do"
  return Complete
