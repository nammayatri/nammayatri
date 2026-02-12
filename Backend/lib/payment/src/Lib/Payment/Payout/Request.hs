{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Request
  ( PayoutRequest (..),
    PayoutRequestStatus (..),
    createPayoutRequest,
    getPayoutRequestById,
    getPayoutRequestByEntity,
    updateStatusWithHistoryById,
    createInitialHistory,
    markCashPending,
    markCashPaid,
    cancelPayoutWithin,
    retryPayoutWith,
    toPaymentState,
    getStatusMessage,
  )
where

import Control.Applicative ((<|>))
import Data.Time.Clock (addUTCTime)
import Kernel.Prelude
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (getCurrentTime, throwError)
import qualified Lib.Finance.Domain.Types.StateTransition as ST
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Payment.Domain.Types.Common as DCommon
import Lib.Payment.Domain.Types.PayoutRequest
import qualified Lib.Payment.Payout.History as PayoutHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR

createPayoutRequest ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequest ->
  m ()
createPayoutRequest payoutRequest = do
  QPR.create payoutRequest
  createInitialHistory payoutRequest

getPayoutRequestById ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Id PayoutRequest ->
  m (Maybe PayoutRequest)
getPayoutRequestById = QPR.findById

getPayoutRequestByEntity ::
  (PaymentBeamFlow.BeamFlow m r) =>
  Maybe DCommon.EntityName ->
  Text ->
  m (Maybe PayoutRequest)
getPayoutRequestByEntity entityName entityId = QPR.findByEntity entityId entityName

updateStatusWithHistoryById ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
updateStatusWithHistoryById newStatus message payoutRequest = do
  QPR.updateStatusById newStatus payoutRequest.id
  recordHistory (Just payoutRequest.status) newStatus message payoutRequest

createInitialHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequest ->
  m ()
createInitialHistory payoutRequest = do
  recordHistory Nothing payoutRequest.status (Just $ getStatusMessage payoutRequest.status) payoutRequest

markCashPending ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  PayoutRequest ->
  m ()
markCashPending agentId agentName mbMessage payoutRequest = do
  now <- getCurrentTime
  QPR.updateCashDetailsById (Just agentId) (Just agentName) (Just now) payoutRequest.id
  updateStatusWithHistoryById CASH_PENDING (mbMessage <|> Just ("Cash Pending marked by " <> agentName)) payoutRequest

markCashPaid ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  PayoutRequest ->
  m ()
markCashPaid agentId agentName mbMessage payoutRequest = do
  now <- getCurrentTime
  QPR.updateCashDetailsById (Just agentId) (Just agentName) (Just now) payoutRequest.id
  updateStatusWithHistoryById CASH_PAID (mbMessage <|> Just ("Cash Paid marked by " <> agentName)) payoutRequest

cancelPayoutWithin ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  NominalDiffTime ->
  Text ->
  PayoutRequest ->
  m ()
cancelPayoutWithin window reason payoutRequest = do
  now <- getCurrentTime
  let cutoffTime = addUTCTime window payoutRequest.createdAt
  unless (payoutRequest.status == INITIATED) $
    throwError $ InvalidRequest "Can only cancel INITIATED payouts"
  unless (now < cutoffTime) $
    throwError $ InvalidRequest "Cancel window expired"
  QPR.updateStatusWithReasonById CANCELLED (Just reason) payoutRequest.id
  recordHistory (Just payoutRequest.status) CANCELLED (Just $ "Cancelled: " <> reason) payoutRequest

retryPayoutWith ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  (PayoutRequestStatus -> Bool) ->
  (PayoutRequest -> m ()) ->
  PayoutRequest ->
  m ()
retryPayoutWith canRetry executePayout payoutRequest = do
  unless (canRetry payoutRequest.status) $
    throwError $ InvalidRequest "Payout is not eligible for retry"
  let nextRetryCount = Just $ fromMaybe 0 payoutRequest.retryCount + 1
  QPR.updateRetryCountById nextRetryCount payoutRequest.id
  updateStatusWithHistoryById RETRYING (Just "Admin initiated retry...") payoutRequest
  executePayout payoutRequest

recordHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Maybe PayoutRequestStatus ->
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
recordHistory fromStatus toStatus message payoutRequest = do
  let merchantIdText = fromMaybe "UNKNOWN" payoutRequest.merchantId
      opCityIdText = fromMaybe "UNKNOWN" payoutRequest.merchantOperatingCityId
  void $
    PayoutHistory.recordPayoutHistory
      PayoutHistory.PayoutHistoryRecord
        { entityType = "PayoutRequest",
          entityId = payoutRequest.id.getId,
          fromState = toPaymentState <$> fromStatus,
          toState = toPaymentState toStatus,
          message = message,
          metadata = Nothing,
          actorType = "SYSTEM",
          actorId = Nothing,
          merchantId = merchantIdText,
          merchantOperatingCityId = opCityIdText
        }

toPaymentState :: PayoutRequestStatus -> ST.PaymentState
toPaymentState INITIATED = ST.INITIATED
toPaymentState PROCESSING = ST.PROCESSING
toPaymentState CREDITED = ST.CREDITED
toPaymentState AUTO_PAY_FAILED = ST.AUTO_PAY_FAILED
toPaymentState RETRYING = ST.RETRYING
toPaymentState FAILED = ST.FAILED
toPaymentState CANCELLED = ST.CANCELLED
toPaymentState CASH_PAID = ST.CASH_PAID
toPaymentState CASH_PENDING = ST.CASH_PENDING

getStatusMessage :: PayoutRequestStatus -> Text
getStatusMessage INITIATED = "Payout scheduled"
getStatusMessage PROCESSING = "Payment in progress"
getStatusMessage CREDITED = "Payment credited to bank"
getStatusMessage AUTO_PAY_FAILED = "Auto-pay failed"
getStatusMessage CANCELLED = "Payment cancelled"
getStatusMessage CASH_PAID = "Payment marked as cash paid"
getStatusMessage CASH_PENDING = "Payment marked as cash pending"
getStatusMessage RETRYING = "Retrying payment..."
getStatusMessage FAILED = "Payment failed/cancelled"
