-- | R77: a SYSTEM cancel (R63 finding timeout) whose refund failed to start used to be a line in the log
-- and a paid rider owed money forever (`SharedLogic.SharedCab.FindingTimeout.startRefund`'s failure branch).
-- This module is the retry ledger for that failure: a short marker in the shared (cross-app) cell records the
-- booking and the attempt count, and `SharedLogic.Scheduler.Jobs.SharedCabDegradedSweep` sweeps pending
-- markers on its existing per-city chain (DegradedSweepSchedule's claim/lease wiring -- R77 deliberately
-- gains NO new job): one retry pass per city per tick, attempts capped at `maxRefundRetryAttempts`, an
-- ops-alert (logWarning + logError) at the cap.
--
-- Keys, unprefixed (Booking.shared master cell): `sharedcab:refundretry:{bookingId}` holds the attempt
-- count (Int, TTL `refundRetryTtlSec`), and `sharedcab:refundretry:city:{merchantOperatingCityId}` is the
-- SET of pending booking ids the city's sweep drains (same TTL, refreshed on every mark). A cleared retry
-- deletes the counter key and drops the id from the set; a GiveUp does the same, so the alert fires once
-- per booking.
module SharedLogic.SharedCab.RefundRetry
  ( refundRetryKey,
    refundRetryCityKey,
    refundRetryTtlSec,
    maxRefundRetryAttempts,
    RetryStep (..),
    retryStep,
    -- batch9 H2: the sweep's idempotency decide, pure and unit-tested across the state matrix
    PaymentState (..),
    RetryAction (..),
    decideRetryStep,
    markRefundRetry,
    pendingRefundRetries,
    readRefundRetryAttempts,
    bumpRefundRetry,
    clearRefundRetry,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler (JobCreator)
import SharedLogic.SharedCab.Booking (shared)
import SharedLogic.SharedCab.DegradedSweepSchedule (ensureDegradedSweep)

-- | Two days: long enough for a payment-gateway outage to pass, short enough that a forgotten marker
-- cannot resurrect an ancient refund.
refundRetryTtlSec :: Int
refundRetryTtlSec = 2 * 24 * 3600

-- | R77: sweep retries of a failed refund start, before ops is alerted and the booking is left to ops.
maxRefundRetryAttempts :: Int
maxRefundRetryAttempts = 5

refundRetryKey :: Id DFTB.FRFSTicketBooking -> Text
refundRetryKey bookingId = "sharedcab:refundretry:" <> bookingId.getId

refundRetryCityKey :: Id DMOC.MerchantOperatingCity -> Text
refundRetryCityKey mocId = "sharedcab:refundretry:city:" <> mocId.getId

-- | One sweep visit of one marked booking, before any effect runs (pure, so the cap is testable):
-- at `maxRefundRetryAttempts` prior attempts the sweep stops retrying and alerts; below it the sweep
-- attempts and writes back the new count.
data RetryStep = AttemptRefund Int | GiveUp
  deriving (Show, Eq)

retryStep :: Int -> Int -> RetryStep
retryStep maxAttempts attemptsDone
  | attemptsDone >= maxAttempts = GiveUp
  | otherwise = AttemptRefund (attemptsDone + 1)

-- | batch9 H2/H3: what the refund pass knows about the marked booking's refund BEFORE it decides anything
-- -- the marker can outlive the refund it was armed for (the push side raced it, ops refunded by hand, a
-- sibling pass started it). The sweep fills this from the payment order's refund records FIRST, then the
-- booking-payment row's status (SharedLogic.Scheduler.Jobs.SharedCabDegradedSweep.refundPaymentState).
data PaymentState
  = -- | the payment order already carries a refund record (the same evidence refundWithAmount /
    -- createRefundService consult before creating one): Done whatever the status column says
    PaymentRefundRecord
  | -- | no refund record on this read, but the payment row says REFUND_INITIATED: that status is stamped
    -- ONLY from an existing refunds row ("refund_api_call_success" in SharedLogic.Payment's
    -- bookingsRefundStatusHandler), so the record exists in transit -- Done
    PaymentRefundInitiatedNoRecord
  | -- | no refund record on this read, but the payment row says REFUNDED: settled -- done already
    PaymentRefundedNoRecord
  | -- | no refund record, payment row says REFUND_PENDING: the H3 stale mark.
    -- markRefundPendingAndSyncOrderStatus stamps REFUND_PENDING BEFORE syncOrderStatus has created the
    -- refund row, so a gateway-side failure leaves exactly this shape: money silently un-refunded were it
    -- read as "started". With NO record it is an owed refund; another startRefund is idempotent here
    -- (createRefundService's one-refund-per-order guard covers a record landing in the race window)
    PaymentRefundPendingNoRecord
  | -- | no payment row at all: terminal -- this is the free-booking shape, or data loss; either way nothing
    -- can be retried into existence (same conclusion as the cancel-start path: "no payment to refund")
    PaymentMissing
  | -- | a payment row, no refund evidence: the refund is genuinely owed -- retry the start
    PaymentOwed
  deriving (Show, Eq)

-- | What one sweep visit of one marked booking does, BEFORE any effect runs (pure, so the matrix is testable):
data RetryAction
  = -- | call FindingTimeout.startRefund (the only action that touches the payment service)
    Start
  | -- | unmark only: no new startRefund, no re-mark of the payment status
    Done
  | -- | unmark + warn, and the marker never loops again
    Terminal
  deriving (Show, Eq)

-- | The idempotency decide (batch9 H2, record-over-status per H3): ANY refund record on the order is DONE,
-- whatever the status column says; REFUND_INITIATED/REFUNDED without a record on this read are still DONE
-- (both statuses are only ever stamped from a refunds row in transit); REFUND_PENDING with NO record is the
-- H3 pre-gateway stale mark -- START again, the refund is owed; a missing payment row is TERMINAL -- the
-- refund pass must NOT loop markers forever on bookings that can never be refunded; and the plain owed
-- state retries the start. Every state maps somewhere: the matrix has no fall-through.
decideRetryStep :: PaymentState -> RetryAction
decideRetryStep = \case
  PaymentRefundRecord -> Done
  PaymentRefundInitiatedNoRecord -> Done
  PaymentRefundedNoRecord -> Done
  PaymentRefundPendingNoRecord -> Start
  PaymentMissing -> Terminal
  PaymentOwed -> Start

-- | The refund did not start: pin the booking to its city's pending set and (re-)seed the city's
-- degraded-sweep chain so the marker is even picked up at all. Idempotent: an already-marked booking
-- keeps its attempt count -- the finding-timeout cancel happens once per booking, and the sweep's own
-- failed visits count through `bumpRefundRetry` instead.
markRefundRetry ::
  (Redis.HedisFlow m r, MonadFlow m, JobCreator r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DFTB.FRFSTicketBooking ->
  m ()
markRefundRetry merchantId mocId bookingId = do
  existing <- shared $ Redis.safeGet @Int (refundRetryKey bookingId)
  when (isNothing existing) $ shared $ Redis.setExp (refundRetryKey bookingId) (0 :: Int) refundRetryTtlSec
  shared $ Redis.sAdd (refundRetryCityKey mocId) [bookingId.getId]
  shared $ Redis.expire (refundRetryCityKey mocId) refundRetryTtlSec
  ensureDegradedSweep merchantId mocId

pendingRefundRetries :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m [Id DFTB.FRFSTicketBooking]
pendingRefundRetries mocId = map Id <$> shared (Redis.sMembers (refundRetryCityKey mocId))

readRefundRetryAttempts :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> m Int
readRefundRetryAttempts = fmap (fromMaybe 0) . shared . Redis.safeGet . refundRetryKey

-- | After a failed attempt of the sweep: the count moves to `attempts` (already incremented past
-- AttemptRefund) and the TTL resets, so an always-failing refund lives out its cap inside the marker TTL.
bumpRefundRetry :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> Int -> m ()
bumpRefundRetry bookingId attempts = shared $ Redis.setExp (refundRetryKey bookingId) attempts refundRetryTtlSec

clearRefundRetry :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Id DFTB.FRFSTicketBooking -> m ()
clearRefundRetry mocId bookingId = do
  shared $ Redis.del (refundRetryKey bookingId)
  shared $ void $ Redis.srem (refundRetryCityKey mocId) [bookingId.getId]
