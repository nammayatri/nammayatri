-- | The payout_transaction row of a bulk order: a copy of the bank details the order was sent with,
-- since driver_bank_account only holds the current account. One row per order, keyed on the short
-- id HDFC echoes back (custrefno), with payoutOrderId set to the order id as the Juspay flow does.
module Lib.Payment.Payout.Bulk.Transaction
  ( recordBulkTransactions,
    updateBulkOrderOutcome,
  )
where

import Control.Applicative ((<|>))
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutTransaction as DPayoutTransaction
import Lib.Payment.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutTransaction as QPayoutTransaction

-- | Record what is about to be sent, before the call: a timed-out file may still have landed and
--   been paid. Only for reporting, so a failure is logged and never stops the payout.
recordBulkTransactions ::
  (MonadFlow m, BeamFlow m r) =>
  DPayoutBatch.PayoutBatchRail ->
  [(DPayoutOrder.PayoutOrder, IPayout.BulkPayoutItem)] ->
  m ()
recordBulkTransactions rail items = do
  res <- withTryCatch "recordBulkTransactions" $ do
    now <- getCurrentTime
    -- An order already recorded keeps its first row.
    unrecorded <- filterM (fmap isNothing . QPayoutTransaction.findByTransactionRef . (.itemRef) . snd) items
    transactions <- forM unrecorded $ \(order, item) -> do
      transactionId <- generateGUID
      pure
        DPayoutTransaction.PayoutTransaction
          { id = transactionId,
            payoutOrderId = Id order.orderId,
            transactionRef = item.itemRef,
            gateWayRefId = Nothing,
            fulfillmentMethod = Just (show rail),
            amount = order.amount,
            status = show order.status,
            merchantId = order.merchantId,
            merchantOperatingCityId = order.merchantOperatingCityId,
            beneficiaryName = Just item.beneficiaryName,
            beneficiaryAccount = Just item.bankAccountNumber,
            beneficiaryIfsc = Just item.bankIfscCode,
            createdAt = now,
            updatedAt = now
          }
    QPayoutTransaction.createMany transactions
  case res of
    Left err -> logError $ "BulkPayout: could not record the bank details of " <> show (length items) <> " item(s): " <> show err
    Right () -> pure ()

-- | 'QPayoutOrder.updateBulkSettled', plus the order's status and bank reference (UTR or FT number)
--   copied onto its payout_transaction row. The order write comes first and is never undone by the copy.
updateBulkOrderOutcome ::
  (MonadFlow m, BeamFlow m r) =>
  JPayout.PayoutOrderStatus ->
  Maybe IPayout.TransferStatus ->
  Maybe Text ->
  Maybe IPayout.SettlementRefType ->
  Maybe Text ->
  Maybe Text ->
  DPayoutOrder.PayoutOrder ->
  m ()
updateBulkOrderOutcome status transferStatus settlementRef settlementRefType responseCode responseMessage order = do
  QPayoutOrder.updateBulkSettled status transferStatus settlementRef settlementRefType responseCode responseMessage order.id
  whenJust order.shortId $ \shortId -> do
    res <-
      withTryCatch "updateBulkOrderOutcome:transaction" $
        QPayoutTransaction.findByTransactionRef (getShortId shortId) >>= \case
          Just transaction ->
            QPayoutTransaction.updateByPrimaryKey
              transaction
                { DPayoutTransaction.status = show status,
                  DPayoutTransaction.gateWayRefId = settlementRef <|> transaction.gateWayRefId
                }
          Nothing -> pure ()
    case res of
      Left err -> logError $ "BulkPayout: could not update the payout transaction of order " <> order.orderId <> ": " <> show err
      Right () -> pure ()
