-- | Per-city sweep that ends degraded rides whose rider never polls back, calling the rule owner (Degraded.expireDegradedBoardingIfNeeded).
-- Candidates are bounded by a [now-3h, now) updatedAt window (h = degradedTimeoutSec): a sweep-down gap shorter than 2h loses nothing.
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
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import qualified SharedLogic.SharedCab.FindingTimeout as FindingTimeout
import qualified SharedLogic.SharedCab.Notify as Notify
import qualified SharedLogic.SharedCab.RefundRetry as RefundRetry
import SharedLogic.SharedCab.Schedule (claimRefundRetryRun, claimSweepRun, scanWindowStart, scheduleNextSweep, withSweepLease)
import qualified SharedLogic.SharedCab.Session as Session
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingExtra as QBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment

pageSize :: Int
pageSize = 200

-- | A pass's vote on the chain's lapse rule: PassOff (duplicate chain holds the claim), PassEmpty (no candidates), PassBusy (work seen or crashed).
data PassOutcome = PassOff | PassEmpty | PassBusy
  deriving (Eq, Show)

type SweepFlow m r c = (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, MonadMask m, FindingTimeout.CancelFlow m r c)

sharedCabDegradedSweep ::
  SweepFlow m r c =>
  Job 'SharedCabDegradedSweep ->
  m ExecutionResult
sharedCabDegradedSweep Job {jobInfo} = do
  let SharedCabDegradedSweepJobData {merchantId, merchantOperatingCityId} = jobInfo.jobData
  enabled <- sharedCabEnabled merchantOperatingCityId
  when enabled $ do
    swept <- withTryCatch "sharedCabDegradedSweep" (sweepCity merchantOperatingCityId)
    case swept of
      -- an empty window in all passes lets the chain lapse; a new degrade, session open or refund marker re-seeds it
      Right (Just [scan, refund])
        | scan /= PassBusy && refund /= PassBusy ->
          logInfo "sharedCab degraded sweep: chain ends (every enabled pass found an empty workspace; a new degrade, session open or refund marker re-seeds it)"
      _ -> scheduleNextSweep merchantId merchantOperatingCityId
  pure Complete

-- | The city's window, under its lease. Each pass is claimed and wrapped in its own try, so a throw costs only that pass this tick.
sweepCity ::
  SweepFlow m r c =>
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
    isolatedPass tag action =
      withTryCatch tag action >>= \case
        Left err -> PassBusy <$ logError (tag <> " pass aborted: " <> show err)
        Right outcome -> pure outcome
    -- a dropped ride stays booking-CONFIRMED, so one batched ticket read keeps only bookings with a rider still INPROGRESS
    onBoardOnly page = do
      tickets <- QTicket.findAllByTicketBookingIds (map (.id) page)
      let boarded = [t.frfsTicketBookingId | t <- tickets, t.status == TicketStatus.INPROGRESS]
      pure [b | b <- page, b.id.getId `elem` map (.getId) boarded]
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
          ended' = ended + length (filter identity flips)
      case lastMay page of
        -- keyset tie at the page boundary: rows sharing it are skipped this sweep, the next full-window scan picks them up
        Just lastRow | length page >= pageSize -> sweepPages fetch narrow decide windowStart endAt (Just lastRow.updatedAt) (scanned', ended')
        _ -> pure (scanned', ended')

-- | The refund pass, on this chain's claim/lease wiring with its own per-tick claim: re-attempts FindingTimeout.startRefund per marked
-- booking, bumps the count on failure, ops-alerts after `maxRefundRetryAttempts`. Decides from the payment evidence BEFORE any
-- startRefund, so an already-started refund is never started twice; a missing payment row is terminal.
refundPass ::
  SweepFlow m r c =>
  Id DMOC.MerchantOperatingCity ->
  m PassOutcome
refundPass mocId = do
  pending <- RefundRetry.pendingRefundRetries mocId
  claimed <- claimRefundRetryRun mocId
  if not claimed
    then pure (if null pending then PassEmpty else PassBusy)
    else do
      pendingAfter <- (sum :: [Int] -> Int) <$> forM pending (\bookingId -> withTryCatch "sharedCab:refundRetry" (retryOne bookingId) >>= either (\err -> 1 <$ logError ("shared-cab refund retry failed for booking " <> bookingId.getId <> ": " <> show err)) pure)
      logInfo $ "sharedCab degraded sweep: city=" <> mocId.getId <> " refund pending=" <> show pendingAfter
      pure $ if pendingAfter == 0 then PassEmpty else PassBusy
  where
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
                  RefundRetry.Done -> do
                    logInfo $ "shared-cab refund retry: booking " <> bookingId.getId <> "'s refund evidence already exists (status or refund record; decideRetryStep Done); unmarking without a new startRefund"
                    0 <$ RefundRetry.clearRefundRetry mocId bookingId
                  -- a charged booking's refund can never self-heal here: an ops event, so it logs as an OPS-ALERT error
                  RefundRetry.Terminal -> do
                    logError $ "OPS-ALERT: shared-cab refund retry: booking " <> bookingId.getId <> " has no payment to refund (decideRetryStep Terminal); unmarking, no more retries -- hand to ops"
                    0 <$ RefundRetry.clearRefundRetry mocId bookingId
                  RefundRetry.Start -> do
                    started <- FindingTimeout.startRefund b
                    if started
                      then do
                        RefundRetry.clearRefundRetry mocId bookingId
                        Notify.notifyFindingTimeout b
                        pure 0
                      else 1 <$ RefundRetry.bumpRefundRetry bookingId n

-- | Refund evidence in ground-truth order: the order's refund records FIRST, then the payment row's status. REFUND_PENDING is stamped
-- BEFORE the refund row exists, so only REFUND_INITIATED/REFUNDED are trusted without a record.
refundPaymentState ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DFTB.FRFSTicketBooking ->
  m RefundRetry.PaymentState
refundPaymentState b =
  QFRFSTicketBookingPayment.findTicketBookingPayment b >>= \case
    Nothing -> pure RefundRetry.PaymentMissing
    Just payment -> do
      mbRefundRecord <-
        QPaymentOrder.findById payment.paymentOrderId >>= \case
          Nothing -> pure Nothing
          Just paymentOrder -> HQRefunds.findLatestByOrderId paymentOrder.shortId
      case mbRefundRecord of
        Just _ -> pure RefundRetry.PaymentRefundRecord
        Nothing
          | payment.status == DTBP.REFUND_INITIATED -> pure RefundRetry.PaymentRefundInitiatedNoRecord
          | payment.status == DTBP.REFUNDED -> pure RefundRetry.PaymentRefundedNoRecord
          | payment.status == DTBP.REFUND_PENDING -> pure RefundRetry.PaymentRefundPendingNoRecord
          | otherwise -> pure RefundRetry.PaymentOwed
