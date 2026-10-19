-- | Booking-fee ledger primitives kept free of SharedLogic imports (SharedLogic.Payment needs them).
module SharedLogic.BookingDepositLedger
  ( bookingDepositRefundRefType,
    withRiderFeeLock,
    withDepositRefundLock,
    withDepositRefundCallLock,
    claimDepositRefundCall,
    resolveDepositRefundLegs,
    unknownRefundOutcome,
    neverRecordedRefund,
  )
where

import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Ledger.Service as Ledger
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import Storage.Beam.Payment ()

bookingDepositRefundRefType :: Text
bookingDepositRefundRefType = "BOOKING_DEPOSIT_REFUND"

feeLockTtlSeconds :: Int
feeLockTtlSeconds = 10

feeBalanceLockKey :: Id DP.Person -> Text
feeBalanceLockKey riderId = "BookingDeposit:Balance:" <> riderId.getId

-- | Cross-app: the API server and the scheduler (expiry, refund-status and sweep jobs) must exclude each other.
withRiderFeeLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DP.Person -> m a -> m a
withRiderFeeLock riderId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis "bookingDeposit" "waitForRiderFeeLock" (feeBalanceLockKey riderId) feeLockTtlSeconds 10000

-- | Shared by server and scheduler; only the lock is cross-app, so keys used inside keep their prefix.
withDepositRefundLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DOrder.PaymentOrder -> m a -> m a
withDepositRefundLock orderId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis "bookingDeposit" "waitForDepositRefundLock" ("BookingDeposit:RefundRequest:" <> orderId.getId) 60 10000

-- | Held for a deposit refund's whole claim-call-record, and awaited by refund status updates on the same
--   order: a status sync during the gateway call would otherwise find no linked request, settle nothing, and
--   then be overwritten by the call's own (older) answer. Cross-app, like the other deposit locks.
withDepositRefundCallLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DOrder.PaymentOrder -> m a -> m a
withDepositRefundCallLock orderId =
  Redis.withWaitAndLockMasterCloudCrossAppRedis "bookingDeposit" "waitForDepositRefundCall" ("BookingDeposit:RefundCallInFlight:" <> orderId.getId) 60 10000

claimDepositRefundCall :: (Redis.HedisFlow m r, MonadFlow m) => Text -> m Bool
claimDepositRefundCall refundRequestId = Redis.runInMasterCloudRedisCellWithCrossAppRedis $ do
  let key = "BookingDeposit:RefundCall:" <> refundRequestId
  won <- Redis.setNxExpire key 400 ("1" :: Text)
  if won
    then pure True
    else do
      holder <- Redis.get @Text key
      when (isNothing holder) $
        throwError $ InternalError ("Could not record the refund-call claim for refund request " <> refundRequestId)
      pure False

-- | A refund row whose API call threw: recorded FAILURE, but the gateway may still have taken it.
unknownRefundOutcome :: DRefunds.Refunds -> Bool
unknownRefundOutcome r =
  r.status == Payment.REFUND_FAILURE && r.isApiCallSuccess == Just False && r.idAssignedByServiceProvider == Nothing

-- | A refund row written before its API call whose call never recorded an answer (the process died mid-call).
neverRecordedRefund :: DRefunds.Refunds -> Bool
neverRecordedRefund r =
  r.status == Payment.REFUND_PENDING && isNothing r.isApiCallSuccess && isNothing r.idAssignedByServiceProvider

-- | PENDING legs only: voidEntry would also void a SETTLED leg.
resolveDepositRefundLegs ::
  (CacheFlow m r, EsqDBFlow m r, HasActorInfo m r, MonadMask m) =>
  Id DP.Person ->
  Id DOrder.PaymentOrder ->
  Payment.RefundStatus ->
  m ()
resolveDepositRefundLegs riderId orderId refundStatus =
  withRiderFeeLock riderId $ do
    pending <- filter (\e -> e.status == LE.PENDING) <$> Ledger.getEntriesByReference bookingDepositRefundRefType orderId.getId
    case refundStatus of
      Payment.REFUND_SUCCESS -> forM_ pending $ \e -> Ledger.settleEntry e.id
      s
        | s `elem` [Payment.REFUND_FAILURE, Payment.REFUND_CANCELED] ->
          forM_ pending $ \e -> Ledger.voidEntry e.id "booking deposit refund failed at gateway"
      _ -> pure ()
