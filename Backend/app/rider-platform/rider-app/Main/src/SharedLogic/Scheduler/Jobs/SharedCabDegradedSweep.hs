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
--
-- batch9: the chain's passes are now independent under one lease -- each claims with its OWN gate folded
-- into the claim (degraded+plated scans: sharedCabDegradedSweepEnabled; the R77 refund pass:
-- sharedCabAllocationEnabled), each is wrapped in its own try so one's throw can't starve the other, and
-- the chain lapses only when no pass reports work left. The refund pass decides idempotently from the
-- booking's payment evidence (RefundRetry.decideRetryStep) BEFORE it calls startRefund (H2).
module SharedLogic.Scheduler.Jobs.SharedCabDegradedSweep
  ( sharedCabDegradedSweep,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import qualified Domain.Types.FRFSTicketBookingStatus as DFTBStatus
import qualified Domain.Types.FRFSTicketStatus as TicketStatus
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.HistoryQueries.Refunds as HQRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Booking (isSharedCabBooking)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as Degraded
import SharedLogic.SharedCab.DegradedSweepSchedule
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.FindingTimeout as FindingTimeout
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import qualified SharedLogic.SharedCab.Session as Session
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingExtra as QBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment

-- | Candidates per page; a page is read-only, the rule owner re-reads under the booking lock.
pageSize :: Int
pageSize = 200

-- | One sweep pass's contribution to the chain's lapse rule (batch9 H1):
--
--   * PassOff -- the pass's own gate is folded into its claim (DegradedSweepSchedule) and closed, or a
--     duplicate chain grabbed this tick's claim: the pass contributes nothing and must neither hold the
--     chain up nor drain it for the other pass.
--   * PassEmpty -- the pass ran (or contested a claim over an empty workspace) and saw no candidates.
--   * PassBusy -- work was seen or the pass crashed mid-tick: the chain must outlive it, and the next tick
--     retries.
data PassOutcome = PassOff | PassEmpty | PassBusy
  deriving (Eq, Show)

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
  swept <- withTryCatch "sharedCabDegradedSweep" (sweepCity merchantOperatingCityId)
  case swept of
    -- an empty window in both passes is the full candidate space for now: let the chain lapse; a new
    -- degrade or session open re-seeds it. A crash, or another city's shard holding the lease, reschedules.
    -- (R77: "both passes" is now three -- degraded, plated, refund-pending -- and a new refund marker re-seeds too.)
    -- (batch9 H1: "both passes" is now per-gate -- lapse means no ENABLED pass reported work; a gated-off
    -- pass contributes nothing, a duplicate chain contributing nothing dies here as it always did.)
    Right (Just [scan, refund])
      | scan /= PassBusy && refund /= PassBusy ->
        logInfo "sharedCab degraded sweep: chain ends (every enabled pass found an empty workspace; a new degrade, session open or refund marker re-seeds it)"
    _ -> scheduleNextSweep merchantId merchantOperatingCityId
  pure Complete

-- | The city's window, under its lease. Per-candidate failure is logged and skipped (the next tick
-- retries it); the byte count is the number of rides the rule owner actually ended this sweep.
-- The (degraded, plated) candidate counts come back so the chain can lapse on an empty window; Nothing when
-- another shard holds the lease. (R77: the count triple is now degraded, plated, refund-pending.)
--
-- (batch9 H1: the counts are now per-pass outcomes -- the scan passes and the refund pass are independent
-- units, each claimed by its own gate and each wrapped in its own try (batch9 MED): a throw in one pass is
-- logged as PassBusy for that pass only -- the other pass still runs, and the chain reschedules so the
-- crashed pass is retried next tick rather than starving both.)
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
  m (Maybe [PassOutcome])
sweepCity mocId =
  withSweepLease mocId $ do
    horizonSec <- (.degradedTimeoutSec) <$> Config.getTunables mocId
    now <- getCurrentTime
    let windowStart = scanWindowStart horizonSec now
    scans <- isolatedPass "sharedCabDegradedSweep:scans" (scanPasses mocId windowStart now)
    refund <- isolatedPass "sharedCabDegradedSweep:refund" (refundPass mocId)
    pure [scans, refund]
  where
    -- batch9 MED: each pass is its own try -- a throw logs and costs THAT pass this tick (PassBusy keeps
    -- the chain up, the next tick retries); it must never starve the other pass or the reschedule above.
    isolatedPass tag action =
      withTryCatch tag action >>= \case
        Left err -> PassBusy <$ logError (tag <> " pass aborted: " <> show err)
        Right outcome -> pure outcome
    -- R51: a dropped ride stays booking-CONFIRMED, so the plated page holds every finished ride in the window. One batched
    -- ticket read keeps only bookings with a rider still INPROGRESS; only those cost the session read and the locked re-decide.
    onBoardOnly page = do
      tickets <- QTicket.findAllByTicketBookingIds (map (.id) page)
      let boarded = [t.frfsTicketBookingId | t <- tickets, t.status == TicketStatus.INPROGRESS]
      pure [b | b <- page, b.id.getId `elem` map (.getId) boarded]
    -- The degraded and plated scans (the M8.5 passes), as one gated unit: the claim now refuses on its own
    -- gate (sharedCabDegradedSweepEnabled, DegradedSweepSchedule.claimSweepRun), and
    -- a duplicate chain finds this run claimed and ends; the gate going off ends the chain too.
    scanPasses cityId windowStart now = do
      claimed <- claimSweepRun cityId
      if not claimed
        then pure PassOff
        else do
          (scanned, ended) <- sweepPages QBooking.findSharedCabDegradedCandidates pure Degraded.expireDegradedBoardingIfNeeded windowStart now Nothing (0, 0)
          (scannedP, endedP) <- sweepPages QBooking.findSharedCabPlatedCandidates onBoardOnly Session.dropStrandedRider windowStart now Nothing (0, 0)
          logInfo $
            "sharedCab degraded sweep: city=" <> cityId.getId <> " scanned=" <> show scanned <> " ended=" <> show ended
              <> " plated scanned="
              <> show scannedP
              <> " dropped="
              <> show endedP
          pure $ if scanned == 0 && scannedP == 0 then PassEmpty else PassBusy
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
--
-- (batch9 H1: the pass's gate is its own -- sharedCabAllocationEnabled (refund markers only arise from
-- finding-timeout cancels, which only fire with allocation on) -- and it is folded into claimRefundRetryRun:
-- gated off, the claim is refused and the pass is PassOff, not "pending work".)
-- (batch9 H2: BEFORE any startRefund the pass decides idempotently from the booking's payment evidence
-- (refundPaymentState + RefundRetry.decideRetryStep): a refund already started/settled anywhere else --
-- the row status, or an existing refund record on the order -- unmarks with no new startRefund and no
-- re-mark of the payment status; a MISSING payment row is terminal (same text as the cancel-start path's
-- "has no payment to refund") and never retried; only real "owed" evidence retries the start.)
refundPass ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m, FindingTimeout.CancelFlow m r c) =>
  Id DMOC.MerchantOperatingCity ->
  m PassOutcome
refundPass mocId
  | not sharedCabAllocationEnabled = pure PassOff
  | otherwise = do
    pending <- RefundRetry.pendingRefundRetries mocId
    claimed <- claimRefundRetryRun mocId
    if not claimed
      then pure (if null pending then PassEmpty else PassBusy)
      else do
        pendingAfter <- (sum :: [Int] -> Int) <$> forM pending (\bookingId -> withTryCatch "sharedCab:refundRetry" (retryOne bookingId) >>= either (\err -> 1 <$ logError ("shared-cab refund retry failed for booking " <> bookingId.getId <> ": " <> show err)) pure)
        logInfo $ "sharedCab degraded sweep: city=" <> mocId.getId <> " refund pending=" <> show pendingAfter
        pure $ if pendingAfter == 0 then PassEmpty else PassBusy
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
                action <- RefundRetry.decideRetryStep <$> refundPaymentState b
                case action of
                  -- batch9 H2: no new startRefund, no markFRFSBookingPaymentStatus -- someone else is doing
                  -- (or has done) this refund; the marker has no work left.
                  RefundRetry.Done -> do
                    logInfo $ "shared-cab refund retry: booking " <> bookingId.getId <> "'s refund evidence already exists (status or refund record; decideRetryStep Done); unmarking without a new startRefund"
                    0 <$ RefundRetry.clearRefundRetry mocId bookingId
                  -- aligned with the cancel-start path's missing-payment text: there is nothing to retry
                  -- (a free booking owes no refund), so the marker is terminal, not queued again.
                  RefundRetry.Terminal -> do
                    logWarning $ "shared-cab refund retry: booking " <> bookingId.getId <> " has no payment to refund (decideRetryStep Terminal); unmarking, no more retries"
                    0 <$ RefundRetry.clearRefundRetry mocId bookingId
                  RefundRetry.Start -> do
                    started <- FindingTimeout.startRefund b
                    if started
                      then do
                        RefundRetry.clearRefundRetry mocId bookingId
                        -- N3: cancelOne suppressed THIS push when the refund did not start; the recovered
                        -- attempt sends it now ("cancelled and refunded in full" is now true).
                        Notify.notifyFindingTimeout b
                        pure 0
                      else 1 <$ RefundRetry.bumpRefundRetry bookingId n

-- | batch9 H2: the booking's refund evidence, in the order the cancel-start path would read it: the
-- booking-payment row's status first (a started refund always leaves REFUND_PENDING/REFUND_INITIATED/REFUNDED
-- on it), then the payment order's refund records (the same refund-row evidence refundWithAmount consults
-- before creating one). A missing order row leaves Owed: startRefund's own throw (PaymentOrderNotFound)
-- keeps today's fail-to-cap behavior, which is where the OPS-ALERT fires.
refundPaymentState ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DFTB.FRFSTicketBooking ->
  m RefundRetry.PaymentState
refundPaymentState b =
  QFRFSTicketBookingPayment.findTicketBookingPayment b >>= \case
    Nothing -> pure RefundRetry.PaymentMissing
    Just payment
      | payment.status == DTBP.REFUND_PENDING || payment.status == DTBP.REFUND_INITIATED -> pure RefundRetry.PaymentRefundStarted
      | payment.status == DTBP.REFUNDED -> pure RefundRetry.PaymentRefunded
      | otherwise ->
        QPaymentOrder.findById payment.paymentOrderId >>= \case
          Nothing -> pure RefundRetry.PaymentOwed
          Just paymentOrder ->
            HQRefunds.findLatestByOrderId paymentOrder.shortId >>= \case
              Just _ -> pure RefundRetry.PaymentRefundRecord
              Nothing -> pure RefundRetry.PaymentOwed
