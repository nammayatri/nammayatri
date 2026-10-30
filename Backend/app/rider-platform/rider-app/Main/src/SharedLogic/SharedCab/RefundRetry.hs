-- | Retry ledger for a system cancel whose refund failed to start: markers in the cross-app cell (`sharedcab:refundretry:{bookingId}`
-- attempt count, `sharedcab:refundretry:city:{city}` pending set) retried by the degraded-sweep chain, capped, then an ops-alert.
module SharedLogic.SharedCab.RefundRetry
  ( refundRetryKey,
    refundRetryTtlSec,
    maxRefundRetryAttempts,
    RetryStep (..),
    retryStep,
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
import SharedLogic.SharedCab.Keys (refundRetryCityKey, refundRetryKey)
import SharedLogic.SharedCab.Schedule (ensureDegradedSweep)

-- | Two days: long enough for a gateway outage to pass, short enough that a forgotten marker cannot resurrect an ancient refund.
refundRetryTtlSec :: Int
refundRetryTtlSec = 2 * 24 * 3600

maxRefundRetryAttempts :: Int
maxRefundRetryAttempts = 5

data RetryStep = AttemptRefund Int | GiveUp
  deriving (Show, Eq)

retryStep :: Int -> Int -> RetryStep
retryStep maxAttempts attemptsDone
  | attemptsDone >= maxAttempts = GiveUp
  | otherwise = AttemptRefund (attemptsDone + 1)

data PaymentState
  = -- | the payment order already carries a refund record (the same evidence refundWithAmount /
    PaymentRefundRecord
  | -- | no refund record on this read, but the payment row says REFUND_INITIATED: that status is stamped
    PaymentRefundInitiatedNoRecord
  | -- | no refund record on this read, but the payment row says REFUNDED: settled -- done already
    PaymentRefundedNoRecord
  | -- | no refund record, payment row says REFUND_PENDING: the H3 stale mark.
    -- REFUND_PENDING is stamped BEFORE the refund row exists, so with NO record it is an owed refund (another startRefund is idempotent)
    PaymentRefundPendingNoRecord
  | -- | no payment row at all: terminal -- this is the free-booking shape, or data loss; either way nothing
    PaymentMissing
  | -- | a payment row, no refund evidence: the refund is genuinely owed -- retry the start
    PaymentOwed
  deriving (Show, Eq)

data RetryAction
  = -- | call FindingTimeout.startRefund (the only action that touches the payment service)
    Start
  | -- | unmark only: no new startRefund, no re-mark of the payment status
    Done
  | -- | unmark + warn, and the marker never loops again
    Terminal
  deriving (Show, Eq)

-- | Any refund record on the order is DONE; REFUND_PENDING with no record is a stale pre-gateway mark, so START again; a missing
-- payment row is TERMINAL, or the pass would loop markers forever.
decideRetryStep :: PaymentState -> RetryAction
decideRetryStep = \case
  PaymentRefundRecord -> Done
  PaymentRefundInitiatedNoRecord -> Done
  PaymentRefundedNoRecord -> Done
  PaymentRefundPendingNoRecord -> Start
  PaymentMissing -> Terminal
  PaymentOwed -> Start

-- | The refund did not start: pin the booking to its city's pending set and re-seed the sweep chain. Idempotent: keeps an existing count.
markRefundRetry ::
  (Redis.HedisFlow m r, MonadFlow m, CacheFlow m r, JobCreator r m) =>
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

bumpRefundRetry :: (Redis.HedisFlow m r, MonadFlow m) => Id DFTB.FRFSTicketBooking -> Int -> m ()
bumpRefundRetry bookingId attempts = shared $ Redis.setExp (refundRetryKey bookingId) attempts refundRetryTtlSec

clearRefundRetry :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Id DFTB.FRFSTicketBooking -> m ()
clearRefundRetry mocId bookingId = do
  shared $ Redis.del (refundRetryKey bookingId)
  shared $ void $ Redis.srem (refundRetryCityKey mocId) [bookingId.getId]
