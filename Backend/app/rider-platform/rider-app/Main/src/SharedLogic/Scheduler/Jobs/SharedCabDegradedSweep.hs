-- M8.5 sweep (SharedLogic.SharedCab.Degraded's TODO + 05-allocation-plan §5): a degraded boarding whose
-- rider never polls back would ride forever -- a degraded ride has no session, so no tick watches it and
-- its only clock was the status poll. This per-city chain is that clock's backup: per candidate it calls
-- EXACTLY the rule owner, SharedLogic.SharedCab.Degraded.expireDegradedBoardingIfNeeded, on the same
-- terms as the poll path (Lib.JourneyLeg.Common.FRFS). Nothing here re-decides shouldExpireDegraded,
-- flips a ticket to USED, or emits a Dropped event.
--
-- Candidates (Storage.Queries.FRFSTicketBookingExtra.findSharedCabDegradedCandidates): the city's
-- CONFIRMED SHARED_CAB bookings with no plate, updatedAt (stamped at the degrade) in [now-3h, now) (h = the city's
-- Config.degradedTimeoutSec), paged newest-first on an updatedAt keyset -- bounded, never a full-table
-- walk. The 3x window is the M8.5 scan-window rule (DegradedSweepSchedule.scanWindowStart): killable from
-- degrade+h, visible until degrade+3h, so a sweep-down gap shorter than 2h loses nothing. "INPROGRESS"
-- (the ticket-state half of the task's candidate filter) is decided by the rule owner inside its locked
-- fresh re-read, not pre-filtered here.
module SharedLogic.Scheduler.Jobs.SharedCabDegradedSweep
  ( sharedCabDegradedSweep,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import SharedLogic.SharedCab.DegradedSweepSchedule
import qualified SharedLogic.SharedCab.Events as Events
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicketBooking as QBooking

-- | Candidates per page; a page is read-only, the rule owner re-reads under the booking lock.
pageSize :: Int
pageSize = 200

sharedCabDegradedSweep ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    Events.EventFlow m r,
    MonadMask m,
    JobCreator r m
  ) =>
  Job 'SharedCabDegradedSweep ->
  m ExecutionResult
sharedCabDegradedSweep Job {jobInfo} = do
  let SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
  claimed <- claimSweepRun merchantOperatingCityId
  -- a duplicate chain finds this run claimed and ends; the gate going off ends the chain too
  if claimed && sharedCabDegradedSweepEnabled
    then sweepCity merchantOperatingCityId `finally` scheduleNextSweep merchantId merchantOperatingCityId
    else logInfo "sharedCab degraded sweep: chain ends (another chain ran this tick, or the sweep is gated off)"
  pure Complete

-- | The city's window, under its lease. Per-candidate failure is logged and skipped (the next tick
-- retries it); the byte count is the number of rides the rule owner actually ended this sweep.
sweepCity ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    Events.EventFlow m r,
    MonadMask m
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m ()
sweepCity mocId =
  withSweepLease mocId $ do
    horizonSec <- (.degradedTimeoutSec) <$> Config.getTunables mocId
    now <- getCurrentTime
    let windowStart = scanWindowStart horizonSec now
    (scanned, ended) <- sweepPages windowStart now Nothing (0, 0)
    logInfo $ "sharedCab degraded sweep: city=" <> mocId.getId <> " scanned=" <> show scanned <> " ended=" <> show ended
  where
    sweepPages windowStart endAt mbCursor (scanned, ended) = do
      page <- QBooking.findSharedCabDegradedCandidates mocId windowStart endAt mbCursor (Just pageSize)
      flips <- forM page $ \booking ->
        if not (isSharedCabBooking booking)
          then pure False -- belt: the DB filter is the serviceTierType column; the helper reads routeStationsJson
          else
            withTryCatch "sharedCab:degradedSweep" (Degraded.expireDegradedBoardingIfNeeded booking) >>= \case
              Left err -> False <$ logError ("sharedCab degraded sweep failed for booking " <> booking.id.getId <> ": " <> show err)
              Right didEnd -> pure didEnd
      let scanned' = scanned + length page
          ended' = ended + length (filter (True ==) flips)
      if length page < pageSize
        then pure (scanned', ended')
        else do
          -- keyset tie at the page's updatedAt boundary: rows sharing it are skipped this sweep,
          -- and the next tick's full-window scan picks them up.
          let cursor = (last page).updatedAt
          sweepPages windowStart endAt (Just cursor) (scanned', ended')
