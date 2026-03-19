module Lib.Payment.Refunds.History
  ( recordRefundsHistory,
    getStatusMessage,
  )
where

import Control.Applicative ((<|>))
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.Finance.Domain.Types.StateTransition as ST
import qualified Lib.Finance.Storage.Beam.BeamFlow as FinanceBeamFlow
import qualified Lib.Finance.Storage.Queries.StateTransition as QTransition
import Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import Lib.Payment.Payout.History as PayoutHelper

-- ---------------------------------------------------------------------------
-- History & state helpers
-- ---------------------------------------------------------------------------
recordRefundsHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Payment.RefundStatus ->
  Payment.RefundStatus ->
  Maybe Text ->
  DRefunds.Refunds ->
  m ()
recordRefundsHistory merchantOperatingCityId mbFromStatus toStatus mbMessage refunds = do
  now <- getCurrentTime
  transitionId <- Id <$> generateGUID
  let mbFromState = toPaymentState <$> mbFromStatus
      toState = toPaymentState toStatus
      metadata = Nothing
      eventData' = PayoutHelper.mkEventData (mbMessage <|> Just (getStatusMessage toStatus)) metadata -- TODO common helper
      transition =
        ST.StateTransition
          { id = transitionId,
            entityType = ST.Refunds,
            entityId = refunds.id.getId,
            fromState = fromMaybe toState mbFromState,
            toState = toState,
            event = toPaymentEvent toStatus,
            eventData = eventData',
            actorType = "SYSTEM",
            actorId = Nothing,
            merchantId = refunds.merchantId,
            merchantOperatingCityId = merchantOperatingCityId.getId,
            createdAt = now,
            updatedAt = now
          }
  QTransition.create transition

toPaymentState :: Payment.RefundStatus -> ST.PaymentState
toPaymentState Payment.REFUND_PENDING = ST.PENDING
toPaymentState Payment.REFUND_FAILURE = ST.FAILED
toPaymentState Payment.REFUND_SUCCESS = ST.REFUNDED
toPaymentState Payment.MANUAL_REVIEW = ST.PENDING
toPaymentState Payment.REFUND_CANCELED = ST.CANCELLED
toPaymentState Payment.REFUND_REQUIRES_ACTION = ST.PENDING

getStatusMessage :: Payment.RefundStatus -> Text
getStatusMessage Payment.REFUND_PENDING = "Refund pending"
getStatusMessage Payment.REFUND_FAILURE = "Refund failed"
getStatusMessage Payment.REFUND_SUCCESS = "Refund successful"
getStatusMessage Payment.MANUAL_REVIEW = "Refund under manual review"
getStatusMessage Payment.REFUND_CANCELED = "Refund cancelled"
getStatusMessage Payment.REFUND_REQUIRES_ACTION = "Refund requires additional action"

toPaymentEvent :: Payment.RefundStatus -> ST.PaymentEvent
toPaymentEvent Payment.REFUND_PENDING = ST.REFUND
toPaymentEvent Payment.REFUND_FAILURE = ST.FAIL
toPaymentEvent Payment.REFUND_SUCCESS = ST.REFUND
toPaymentEvent Payment.MANUAL_REVIEW = ST.REFUND
toPaymentEvent Payment.REFUND_CANCELED = ST.CANCEL
toPaymentEvent Payment.REFUND_REQUIRES_ACTION = ST.REFUND
