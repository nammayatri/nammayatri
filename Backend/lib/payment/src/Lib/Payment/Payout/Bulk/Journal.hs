module Lib.Payment.Payout.Bulk.Journal
  ( eventsOfOutcome,
    recordInitiated,
    recordItemEvents,
  )
where

import qualified Kernel.External.Payout.Interface.Types as Payout
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutLifecycleEvent as DPLE
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutLifecycleEvent as QPLE

eventsOfOutcome :: Payout.BulkItemOutcome -> Maybe Payout.TransferStatus -> [DPLE.PayoutLifecycleEventType]
eventsOfOutcome outcome mbSettleStatus = case outcome of
  Payout.ItemPendingApproval {} -> []
  Payout.ItemInterim {} -> []
  Payout.ItemProcessed {} ->
    DPLE.SETTLED_AT_BANK : [DPLE.REVERSED | mbSettleStatus == Just Payout.TRANSFER_FAILED]
  Payout.ItemRejected {} ->
    [DPLE.FAILED]

recordInitiated ::
  (BeamFlow m r, MonadFlow m) =>
  DPayoutBatch.PayoutBatch ->
  [DPayoutOrder.PayoutOrder] ->
  m ()
recordInitiated batch orders = do
  let mbBatchNo = batch.partnerBatchRef
  case mbBatchNo of
    Nothing -> pure ()
    Just batchNo ->
      forM_ orders $ \order ->
        when (order.status /= Payout.FAILURE) $
          writeOnce order.id DPLE.INITIATED $
            mkEntry batchNo batch order DPLE.INITIATED (Just "P/-")

recordItemEvents ::
  (BeamFlow m r, MonadFlow m) =>
  DPayoutBatch.PayoutBatch ->
  DPayoutOrder.PayoutOrder ->
  Payout.BulkItemOutcome ->
  Maybe Payout.TransferStatus ->
  m ()
recordItemEvents batch order outcome mbSettleStatus = do
  let events = eventsOfOutcome outcome mbSettleStatus
      mbBatchNo = batch.partnerBatchRef
      partnerCode = codeOf outcome
  case mbBatchNo of
    Nothing -> pure ()
    Just batchNo ->
      forM_ events $ \eventType ->
        writeOnce order.id eventType $
          mkEntry batchNo batch order eventType partnerCode

writeOnce ::
  (BeamFlow m r, MonadFlow m) =>
  Id DPayoutOrder.PayoutOrder ->
  DPLE.PayoutLifecycleEventType ->
  m DPLE.PayoutLifecycleEvent ->
  m ()
writeOnce orderId eventType mkRow = do
  result <- try @_ @SomeException $ do
    existing <- QPLE.findByPayoutOrderIdAndEventType orderId eventType
    when (isNothing existing) $ do
      row <- mkRow
      QPLE.create row
  case result of
    Right () -> pure ()
    Left err -> logError $ "payout_lifecycle_event_write_failed: orderId=" <> orderId.getId <> " event=" <> show eventType <> " error=" <> show err

mkEntry ::
  (MonadFlow m) =>
  Text ->
  DPayoutBatch.PayoutBatch ->
  DPayoutOrder.PayoutOrder ->
  DPLE.PayoutLifecycleEventType ->
  Maybe Text ->
  m DPLE.PayoutLifecycleEvent
mkEntry batchNo batch order eventType partnerCode = do
  entryId <- generateGUID
  now <- getCurrentTime
  pure
    DPLE.PayoutLifecycleEvent
      { id = Id entryId,
        batchNo = batchNo,
        driverId = order.customerId,
        eventType = eventType,
        batchId = batch.id,
        payoutOrderId = order.id,
        payoutRequestId = listToMaybe =<< order.entityIds,
        amount = order.amount.amount,
        currency = order.amount.currency,
        partnerCode = partnerCode,
        merchantId = batch.merchantId,
        merchantOperatingCityId = batch.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

codeOf :: Payout.BulkItemOutcome -> Maybe Text
codeOf = \case
  Payout.ItemInterim {code} -> code
  Payout.ItemPendingApproval {code} -> code
  Payout.ItemProcessed {code} -> code
  Payout.ItemRejected {code} -> code
