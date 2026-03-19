{-# OPTIONS_GHC -Wno-deprecations #-}

module Lib.Payment.Storage.HistoryQueries.PaymentTransaction
  ( create,
    updateMultiple,
    findByTxnUUID,
    findByTxnId,
    findById,
    findAllByOrderId,
    findNewTransactionByOrderId,
    findEarliestChargedTransactionByOrderId,
    updateStatusAndError,
    updateAmount,
    incrementRetryCountAndError,
  )
where

import Kernel.External.Payment.Juspay.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Payment.Domain.Types.Common
import Lib.Payment.Domain.Types.PaymentOrder (PaymentOrder)
import Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Payment.History as PaymentHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTransaction

-- ---------------------------------------------------------------------------
-- Queries with history
-- ---------------------------------------------------------------------------

type BeamFlow m r = (FinanceBeamFlow.BeamFlow m r, PaymentBeamFlow.BeamFlow m r)

create ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  PaymentTransaction ->
  Maybe Text ->
  m ()
create merchantOpCityId transaction mbAction = do
  let historyMessage =
        "Create transaction: "
          <> PaymentHistory.getStatusMessage transaction.status
          <> maybe "" ("; action: " <>) mbAction
  QTransaction.create transaction
  PaymentHistory.recordPaymentHistory merchantOpCityId Nothing transaction.status (Just historyMessage) transaction

updateMultiple ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  PaymentTransaction ->
  PaymentTransaction ->
  Maybe Text ->
  m ()
updateMultiple merchantOpCityId transaction updTransaction mbAction = do
  let historyMessage =
        "Update multiple: "
          <> PaymentHistory.getStatusMessage updTransaction.status
          <> maybe "" (\action -> "; " <> "action: " <> action) mbAction
  QTransaction.updateMultiple updTransaction
  PaymentHistory.recordPaymentHistory merchantOpCityId (Just transaction.status) updTransaction.status (Just historyMessage) updTransaction

findByTxnUUID :: PaymentBeamFlow.BeamFlow m r => Text -> m (Maybe PaymentTransaction)
findByTxnUUID = QTransaction.findByTxnUUID

findByTxnId :: PaymentBeamFlow.BeamFlow m r => Text -> m (Maybe PaymentTransaction)
findByTxnId = QTransaction.findByTxnId

findById :: PaymentBeamFlow.BeamFlow m r => Id PaymentTransaction -> m (Maybe PaymentTransaction)
findById = QTransaction.findById

findAllByOrderId :: PaymentBeamFlow.BeamFlow m r => Id PaymentOrder -> m [PaymentTransaction]
findAllByOrderId = QTransaction.findAllByOrderId

findNewTransactionByOrderId :: PaymentBeamFlow.BeamFlow m r => Id PaymentOrder -> m (Maybe PaymentTransaction)
findNewTransactionByOrderId = QTransaction.findNewTransactionByOrderId

findEarliestChargedTransactionByOrderId :: PaymentBeamFlow.BeamFlow m r => Id PaymentOrder -> m (Maybe PaymentTransaction)
findEarliestChargedTransactionByOrderId = QTransaction.findEarliestChargedTransactionByOrderId

updateStatusAndError ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  PaymentTransaction ->
  TransactionStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m ()
updateStatusAndError merchantOpCityId transaction status mbErrorCode mbErrorMessage mbAction = do
  let historyMessage =
        "Update status and error: "
          <> PaymentHistory.getStatusMessage status
          <> maybe "" ("; action: " <>) mbAction
          <> maybe "" ("; error code: " <>) mbErrorCode
          <> maybe "" ("; error message: " <>) mbErrorMessage
  QTransaction.updateStatusAndError transaction.id status mbErrorCode mbErrorMessage
  PaymentHistory.recordPaymentHistory merchantOpCityId (Just transaction.status) status (Just historyMessage) transaction

updateAmount ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  PaymentTransaction ->
  HighPrecMoney ->
  HighPrecMoney ->
  Maybe Text ->
  m ()
updateAmount merchantOpCityId transaction newAmount newApplicationFeeAmount mbAction = do
  let historyMessage =
        "Update amount: "
          <> show newAmount
          <> "; applicationFeeAmount: "
          <> show newApplicationFeeAmount
          <> "; old amount: "
          <> show transaction.amount
          <> "; old applicationFeeAmount: "
          <> show transaction.applicationFeeAmount
          <> maybe "" ("; action: " <>) mbAction
  QTransaction.updateAmount transaction.id newAmount newApplicationFeeAmount
  PaymentHistory.recordPaymentHistory merchantOpCityId (Just transaction.status) transaction.status (Just historyMessage) transaction

incrementRetryCountAndError ::
  BeamFlow m r =>
  Id MerchantOperatingCity ->
  PaymentTransaction ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m ()
incrementRetryCountAndError merchantOpCityId transaction mbErrorCode mbErrorMessage mbAction = do
  let historyMessage =
        "Increment retry count: "
          <> show (transaction.retryCount + 1)
          <> maybe "" ("; action: " <>) mbAction
          <> maybe "" ("; error code: " <>) mbErrorCode
          <> maybe "" ("; error message: " <>) mbErrorMessage
  QTransaction.updateRetryCountAndError transaction.id (transaction.retryCount + 1) mbErrorCode mbErrorMessage
  PaymentHistory.recordPaymentHistory merchantOpCityId (Just transaction.status) transaction.status (Just historyMessage) transaction
