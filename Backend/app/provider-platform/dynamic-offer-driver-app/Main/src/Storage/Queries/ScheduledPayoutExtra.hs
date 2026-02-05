module Storage.Queries.ScheduledPayoutExtra where

import qualified Domain.Types.ScheduledPayout as DSP
import Kernel.Beam.Functions (updateOneWithKV)
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.PayoutStatusHistory as LPPS
import qualified Lib.Payment.Storage.Queries.PayoutStatusHistory as QPSH
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledPayout as Beam
import qualified Storage.Queries.ScheduledPayout as QSP

-- | Update ScheduledPayout status and record history entry in one call
-- This centralizes all status updates to also track history
updateStatusWithHistoryById ::
  (BeamFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  LPPS.ScheduledPayoutStatus ->
  Maybe Text ->
  DSP.ScheduledPayout ->
  m ()
updateStatusWithHistoryById newStatus message scheduledPayout = do
  now <- getCurrentTime

  -- 1. Update the ScheduledPayout status
  QSP.updateStatusById newStatus scheduledPayout.id

  -- 2. Create history entry (lib uses Text for scheduledPayoutId and Maybe Text for merchant ids)
  historyId <- Kernel.Types.Id.Id <$> generateGUID
  let historyEntry =
        LPPS.PayoutStatusHistory
          { LPPS.id = historyId,
            LPPS.scheduledPayoutId = Kernel.Types.Id.getId scheduledPayout.id,
            LPPS.status = newStatus,
            LPPS.message = message,
            LPPS.createdAt = now,
            LPPS.updatedAt = now,
            LPPS.merchantId = Kernel.Types.Id.getId <$> scheduledPayout.merchantId,
            LPPS.merchantOperatingCityId = Kernel.Types.Id.getId <$> scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry

-- | Create initial history entry when ScheduledPayout is first created
createInitialHistory ::
  (BeamFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DSP.ScheduledPayout ->
  m ()
createInitialHistory scheduledPayout = do
  historyId <- Kernel.Types.Id.Id <$> generateGUID
  let historyEntry =
        LPPS.PayoutStatusHistory
          { LPPS.id = historyId,
            LPPS.scheduledPayoutId = Kernel.Types.Id.getId scheduledPayout.id,
            LPPS.status = scheduledPayout.status,
            LPPS.message = Just (getStatusMessage scheduledPayout.status),
            LPPS.createdAt = scheduledPayout.createdAt,
            LPPS.updatedAt = scheduledPayout.createdAt,
            LPPS.merchantId = Kernel.Types.Id.getId <$> scheduledPayout.merchantId,
            LPPS.merchantOperatingCityId = Kernel.Types.Id.getId <$> scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry

-- | Status message helpers for consistent history messages
getStatusMessage :: LPPS.ScheduledPayoutStatus -> Text
getStatusMessage LPPS.INITIATED = "Payout scheduled"
getStatusMessage LPPS.PROCESSING = "Payment in progress"
getStatusMessage LPPS.CREDITED = "Payment credited to bank"
getStatusMessage LPPS.AUTO_PAY_FAILED = "Auto-pay failed"
getStatusMessage LPPS.CANCELLED = "Payment cancelled"
getStatusMessage LPPS.CASH_PAID = "Payment marked as cash paid"
getStatusMessage LPPS.CASH_PENDING = "Payment marked as cash pending"
getStatusMessage LPPS.RETRYING = "Retrying payment..."
getStatusMessage LPPS.FAILED = "Payment failed/cancelled"

-- | Cast Juspay payout order status to ScheduledPayoutStatus for Special Zone Payouts
castPayoutOrderStatusToScheduledPayoutStatus :: Payout.PayoutOrderStatus -> LPPS.ScheduledPayoutStatus
castPayoutOrderStatusToScheduledPayoutStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> LPPS.CREDITED
    Payout.FULFILLMENTS_SUCCESSFUL -> LPPS.CREDITED
    Payout.ERROR -> LPPS.AUTO_PAY_FAILED
    Payout.FAILURE -> LPPS.AUTO_PAY_FAILED
    Payout.FULFILLMENTS_FAILURE -> LPPS.AUTO_PAY_FAILED
    Payout.CANCELLED -> LPPS.CANCELLED
    Payout.FULFILLMENTS_CANCELLED -> LPPS.CANCELLED
    Payout.FULFILLMENTS_MANUAL_REVIEW -> LPPS.PROCESSING -- Keep processing for manual review
    _ -> LPPS.PROCESSING

-- | Update the payout transaction ID after receiving it from the payout service
updatePayoutTransactionId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Id DSP.ScheduledPayout ->
  m ()
updatePayoutTransactionId txnId scheduledPayoutId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.payoutTransactionId txnId, Se.Set Beam.updatedAt now]
    [Se.Is Beam.id $ Se.Eq (getId scheduledPayoutId)]
