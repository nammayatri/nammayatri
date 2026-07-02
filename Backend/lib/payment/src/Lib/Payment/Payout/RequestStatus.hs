{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.RequestStatus
  ( castPayoutOrderStatusToPayoutRequestStatus,
    updatePayoutRequestStatusWithHistory,
    recordHistory,
    toPaymentState,
    toPaymentEvent,
    getStatusMessage,
  )
where

import qualified Kernel.External.Payout.Juspay.Types.Payout as JuspayPayout
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.StateTransition as ST
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import Lib.Payment.Domain.Types.PayoutRequest
import qualified Lib.Payment.Payout.History as PayoutHistory
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR

castPayoutOrderStatusToPayoutRequestStatus :: JuspayPayout.PayoutOrderStatus -> PayoutRequestStatus
castPayoutOrderStatusToPayoutRequestStatus = \case
  JuspayPayout.SUCCESS -> CREDITED
  JuspayPayout.FULFILLMENTS_SUCCESSFUL -> CREDITED
  JuspayPayout.ERROR -> AUTO_PAY_FAILED
  JuspayPayout.FAILURE -> AUTO_PAY_FAILED
  JuspayPayout.FULFILLMENTS_FAILURE -> AUTO_PAY_FAILED
  JuspayPayout.CANCELLED -> CANCELLED
  JuspayPayout.FULFILLMENTS_CANCELLED -> CANCELLED
  _ -> PROCESSING

updatePayoutRequestStatusWithHistory ::
  (PaymentBeamFlow.BeamFlow m r, FinanceBeamFlow.BeamFlow m r) =>
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
updatePayoutRequestStatusWithHistory newStatus message payoutRequest = do
  QPR.updateStatusById newStatus payoutRequest.id
  recordHistory (Just payoutRequest.status) newStatus message payoutRequest

recordHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Maybe PayoutRequestStatus ->
  PayoutRequestStatus ->
  Maybe Text ->
  PayoutRequest ->
  m ()
recordHistory fromStatus toStatus message payoutRequest = do
  let merchantIdText = payoutRequest.merchantId
      opCityIdText = payoutRequest.merchantOperatingCityId
  void $
    PayoutHistory.recordPayoutHistory
      PayoutHistory.PayoutHistoryRecord
        { entityType = ST.PayoutRequest,
          entityId = payoutRequest.id.getId,
          fromState = toPaymentState <$> fromStatus,
          toState = toPaymentState toStatus,
          paymentEvent = toPaymentEvent toStatus,
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
getStatusMessage RETRYING = "Retrying payment..."
getStatusMessage FAILED = "Payment failed/cancelled"
getStatusMessage CASH_PAID = "Payment marked as cash paid"
getStatusMessage CASH_PENDING = "Payment marked as cash pending"

toPaymentEvent :: PayoutRequestStatus -> ST.PaymentEvent
toPaymentEvent INITIATED = ST.INITIATE
toPaymentEvent PROCESSING = ST.CAPTURE
toPaymentEvent CREDITED = ST.CREDIT
toPaymentEvent AUTO_PAY_FAILED = ST.FAIL
toPaymentEvent CANCELLED = ST.CANCEL
toPaymentEvent RETRYING = ST.RETRY
toPaymentEvent FAILED = ST.FAIL
toPaymentEvent CASH_PAID = ST.CREDIT
toPaymentEvent CASH_PENDING = ST.INITIATE
