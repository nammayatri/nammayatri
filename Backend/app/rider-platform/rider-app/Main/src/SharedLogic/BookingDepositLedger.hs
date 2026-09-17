-- | Booking-fee ledger primitives kept free of SharedLogic imports (SharedLogic.Payment needs them).
module SharedLogic.BookingDepositLedger
  ( bookingDepositRefundRefType,
    withRiderFeeLock,
    resolveDepositRefundLegs,
  )
where

import qualified Domain.Types.Person as DP
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Finance.Ledger.Service as Ledger
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Storage.Beam.Payment ()

bookingDepositRefundRefType :: Text
bookingDepositRefundRefType = "BOOKING_DEPOSIT_REFUND"

feeLockTtlSeconds :: Int
feeLockTtlSeconds = 10

feeBalanceLockKey :: Id DP.Person -> Text
feeBalanceLockKey riderId = "BookingDeposit:Balance:" <> riderId.getId

withRiderFeeLock :: (Redis.HedisFlow m r, MonadMask m, MonadFlow m) => Id DP.Person -> m a -> m a
withRiderFeeLock riderId =
  Redis.withMasterRedis . Redis.withWaitAndLockRedis (feeBalanceLockKey riderId) feeLockTtlSeconds 10000

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
