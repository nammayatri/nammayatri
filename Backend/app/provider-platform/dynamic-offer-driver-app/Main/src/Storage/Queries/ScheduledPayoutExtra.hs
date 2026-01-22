module Storage.Queries.ScheduledPayoutExtra where

import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.ScheduledPayout as DSP
import Kernel.Beam.Functions (updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledPayout as Beam
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.ScheduledPayout as QSP

-- | Update ScheduledPayout status and record history entry in one call
-- This centralizes all status updates to also track history
updateStatusWithHistoryById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DSP.ScheduledPayoutStatus ->
  Maybe Text ->
  DSP.ScheduledPayout ->
  m ()
updateStatusWithHistoryById newStatus message scheduledPayout = do
  now <- getCurrentTime

  -- 1. Update the ScheduledPayout status
  QSP.updateStatusById newStatus scheduledPayout.id

  -- 2. Create history entry
  historyId <- Id <$> generateGUID
  let historyEntry =
        DPSH.PayoutStatusHistory
          { id = historyId,
            scheduledPayoutId = scheduledPayout.id,
            status = newStatus,
            message = message,
            createdAt = now,
            updatedAt = now,
            merchantId = scheduledPayout.merchantId,
            merchantOperatingCityId = scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry

-- | Create initial history entry when ScheduledPayout is first created
createInitialHistory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DSP.ScheduledPayout ->
  m ()
createInitialHistory scheduledPayout = do
  historyId <- Id <$> generateGUID
  let historyEntry =
        DPSH.PayoutStatusHistory
          { id = historyId,
            scheduledPayoutId = scheduledPayout.id,
            status = scheduledPayout.status,
            message = Just (getStatusMessage scheduledPayout.status),
            createdAt = scheduledPayout.createdAt,
            updatedAt = scheduledPayout.createdAt,
            merchantId = scheduledPayout.merchantId,
            merchantOperatingCityId = scheduledPayout.merchantOperatingCityId
          }
  QPSH.create historyEntry

-- | Status message helpers for consistent history messages
getStatusMessage :: DSP.ScheduledPayoutStatus -> Text
getStatusMessage DSP.INITIATED = "Payout scheduled"
getStatusMessage DSP.PROCESSING = "Payment in progress"
getStatusMessage DSP.CREDITED = "Payment credited to bank"
getStatusMessage DSP.AUTO_PAY_FAILED = "Auto-pay failed"
getStatusMessage DSP.RETRYING = "Retrying payment..."
getStatusMessage DSP.FAILED = "Payment failed/cancelled"

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
