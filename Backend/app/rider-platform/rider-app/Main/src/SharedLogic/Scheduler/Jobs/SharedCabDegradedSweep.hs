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

import qualified Domain.Types.FRFSTicketBookingStatus as DFTBStatus
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import qualified SharedLogic.SharedCab.FindingTimeout as FindingTimeout
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import SharedLogic.SharedCab.DegradedSweepSchedule
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Session as Session
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingExtra as QBooking

-- | Candidates per page; a page is read-only, the rule owner re-reads under the booking lock.
pageSize :: Int
pageSize = 200

sharedCabDegradedSweep ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Events.EventFlow m r,
    MonadMask m,
    JobCreator r m,
    -- R77: the refund pass retries the payment refund of a failed finding-timeout cancel (startRefund)
    FindingTimeout.CancelFlow m r c
  ) =>
  Job 'SharedCabDegradedSweep ->
  m ExecutionResult
sharedCabDegradedSweep Job {jobInfo} = do
  let SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
  claimed <- claimSweepRun merchantOperatingCityId
  -- a duplicate chain finds this run claimed and ends; the gate going off ends the chain too
  if claimed && sharedCabDegradedSweepEnabled
    then do
      swept <- withTryCatch "sharedCabDegradedSweep" (sweepCity merchantOperatingCityId)
      case swept of
        -- an empty window in both passes is the full candidate space for now: let the chain lapse; a new
        -- degrade or session open re-seeds it. A crash, or another city's shard holding the lease, reschedules.
        -- (R77: "both passes" is now three -- degraded, plated, refund-pending -- and a new refund marker re-seeds too.)
        Right (Just (0, 0, 0)) -> logInfo "sharedCab degraded sweep: chain ends (scan window empty; a new degrade, session open or refund marker re-seeds it)"
        _ -> scheduleNextSweep merchantId merchantOperatingCityId
    else logInfo "sharedCab degraded sweep: chain ends (another chain ran this tick, or the sweep is gated off)"
  pure Complete

-- | The city's window, under its lease. Per-candidate failure is logged and skipped (the next tick
-- retries it); the byte count is the number of rides the rule owner actually ended this sweep.
-- The (degraded, plated) candidate counts come back so the chain can lapse on an empty window; Nothing when
-- another shard holds the lease. (R77: the count triple is now degraded, plated, refund-pending.)
sweepCity ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Events.EventFlow m r,
    MonadMask m,
    FindingTimeout.CancelFlow m r c
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m (Maybe (Int, Int, Int))
sweepCity mocId =
  withSweepLease mocId $ do
    horizonSec <- (.degradedTimeoutSec) <$> Config.getTunables mocId
    now <- getCurrentTime
    let windowStart = scanWindowStart horizonSec now
    (scanned, ended) <- sweepPages QBooking.findSharedCabDegradedCandidates pure Degraded.expireDegradedBoardingIfNeeded windowStart now Nothing (0, 0)
    (scannedP, endedP) <- sweepPages QBooking.findSharedCabPlatedCandidates onBoardOnly Session.dropStrandedRider windowStart now Nothing (0, 0)
    refundPending <- refundPass mocId
    logInfo $
      "sharedCab degraded sweep: city=" <> mocId.getId <> " scanned=" <> show scanned <> " ended=" <> show ended
        <> " plated scanned="
        <> show scannedP
        <> " dropped="
        <> show endedP
        <> " refund pending="
        <> show refundPending
    pure (scanned, scannedP, refundPending)
  where
    -- R51: a dropped ride stays booking-CONFIRMED, so the plated page holds every finished ride in the window. One batched
    -- ticket read keeps only bookings with a rider still INPROGRESS; only those cost the session read and the locked re-decide.
    onBoardOnly page = do
      tickets <- QTicket.findAllByTicketBookingIds (map (.id) page)
      let boarded = [t.frfsTicketBookingId | t <- tickets, t.status == TicketStatus.INPROGRESS]
      pure [b | b <- page, b.id.getId `elem` map (.getId) boarded]
    -- Each candidate is re-decided by its rule owner under the locks; a failure is logged and the next tick retries it.
    sweepPages fetch narrow decide windowStart endAt mbCursor (scanned, ended) = do
      page <- fetch mocId windowStart endAt mbCursor (Just pageSize)
      candidates <- narrow page
      flips <- forM candidates $ \booking ->
        if not (isSharedCabBooking booking)
          then pure False -- belt: the DB filter is the serviceTierType column; the helper reads routeStationsJson
          else
            withTryCatch "sharedCab:degradedSweep" (decide booking) >>= \case
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
          sweepPages fetch narrow decide windowStart endAt (Just cursor) (scanned', ended')


-- | M3/N3 + R77: the refund pass, riding this chain's same claim/lease/reschedule wiring (no new job, its own
-- per-tick claim `claimRefundRetryRun`, level with `claimSweepRun`). Candidates are the city's
-- `sharedcab:refundretry:` set: each marked booking gets its refund re-attempted (FindingTimeout.startRefund,
-- same call the original cancel tried), its marker cleared on success (plus the refund push the failure
-- suppressed), its attempt count bumped on failure, and -- `maxRefundRetryAttempts` attempts in -- an
-- ops-alert and no more retries.
--
-- A NON-cancelled booking is unmarked (its refund is some other path's business, e.g. a rider undo); a booking
-- that vanished is dropped from the set. A contested claim returns the pending count un-processed, so the
-- chain still reschedules (this tick's winner owns it).
refundPass ::
  (MonadFlow m, Redis.HedisFlow m r, EsqDBFlow m r, MonadMask m, FindingTimeout.CancelFlow m r c) =>
  Id DMOC.MerchantOperatingCity ->
  m Int
refundPass mocId = do
  pending <- RefundRetry.pendingRefundRetries mocId
  claimed <- claimRefundRetryRun mocId
  if not claimed
    then pure (length pending)
    else sum <$> forM pending (\bookingId -> withTryCatch "sharedCab:refundRetry" (retryOne bookingId) >>= either (\err -> 1 <$ logError ("shared-cab refund retry failed for booking " <> bookingId.getId <> ": " <> show err)) pure)
  where
    -- 1 while the booking still owes a retry, 0 once it is resolved this tick (retried to pay, given up, or unmarked).
    retryOne bookingId = do
      attempts <- RefundRetry.readRefundRetryAttempts bookingId
      case RefundRetry.retryStep RefundRetry.maxRefundRetryAttempts attempts of
        RefundRetry.GiveUp -> do
          logWarning $ "shared-cab refund for booking " <> bookingId.getId <> " still not started after " <> show attempts <> " attempts; giving up"
          logError $ "OPS-ALERT: shared-cab refund for booking " <> bookingId.getId <> " never started after " <> show RefundRetry.maxRefundRetryAttempts <> " attempts; the rider keeps a charged fare -- hand to ops"
          0 <$ RefundRetry.clearRefundRetry mocId bookingId
        RefundRetry.AttemptRefund n ->
          QFRFSTicketBooking.findById bookingId >>= \case
            Nothing -> do
              logError $ "shared-cab refund retry: booking " <> bookingId.getId <> " not found; unmarking"
              0 <$ RefundRetry.clearRefundRetry mocId bookingId
            Just b
              | b.status /= DFTBStatus.CANCELLED -> do
                  logWarning $ "shared-cab refund retry: booking " <> bookingId.getId <> " is " <> show b.status <> ", not CANCELLED (the marker is stale); unmarking"
                  0 <$ RefundRetry.clearRefundRetry mocId bookingId
              | otherwise -> do
                  started <- FindingTimeout.startRefund b
                  if started
                    then do
                      RefundRetry.clearRefundRetry mocId bookingId
                      -- N3: cancelOne suppressed THIS push when the refund did not start; the recovered
                      -- attempt sends it now ("cancelled and refunded in full" is now true).
                      Notify.notifyFindingTimeout b
                      pure 0
                    else 1 <$ RefundRetry.bumpRefundRetry bookingId n
