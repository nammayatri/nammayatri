module Lib.Payment.Payment.History
  ( recordPaymentHistory,
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
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import Lib.Payment.Payout.History as PayoutHelper

-- ---------------------------------------------------------------------------
-- History & state helpers
-- ---------------------------------------------------------------------------
recordPaymentHistory ::
  (FinanceBeamFlow.BeamFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Payment.TransactionStatus ->
  Payment.TransactionStatus ->
  Maybe Text ->
  DTransaction.PaymentTransaction ->
  m ()
recordPaymentHistory merchantOperatingCityId mbFromStatus toStatus mbMessage paymentTransaction = do
  now <- getCurrentTime
  transitionId <- Id <$> generateGUID
  let mbFromState = toPaymentState <$> mbFromStatus
      toState = toPaymentState toStatus
      metadata = Nothing
      eventData' = PayoutHelper.mkEventData (mbMessage <|> Just (getStatusMessage toStatus)) metadata -- TODO common helper
      transition =
        ST.StateTransition
          { id = transitionId,
            entityType = "PaymentTransaction",
            entityId = paymentTransaction.id.getId,
            fromState = fromMaybe toState mbFromState,
            toState = toState,
            event = toPaymentEvent toStatus,
            eventData = eventData',
            actorType = "SYSTEM",
            actorId = Nothing,
            merchantId = paymentTransaction.merchantId.getId,
            merchantOperatingCityId = merchantOperatingCityId.getId,
            createdAt = now,
            updatedAt = now
          }
  QTransition.create transition

toPaymentState :: Payment.TransactionStatus -> ST.PaymentState
toPaymentState Payment.NEW = ST.Pending
toPaymentState Payment.PENDING_VBV = ST.Pending
toPaymentState Payment.CHARGED = ST.Captured
toPaymentState Payment.AUTHENTICATION_FAILED = ST.Failed
toPaymentState Payment.AUTHORIZATION_FAILED = ST.Failed
toPaymentState Payment.CANCELLED = ST.Cancelled
toPaymentState Payment.JUSPAY_DECLINED = ST.Failed
toPaymentState Payment.AUTHORIZING = ST.PROCESSING
toPaymentState Payment.COD_INITIATED = ST.CASH_PENDING
toPaymentState Payment.STARTED = ST.INITIATED
toPaymentState Payment.AUTO_REFUNDED = ST.Refunded
toPaymentState Payment.CLIENT_AUTH_TOKEN_EXPIRED = ST.Failed

getStatusMessage :: Payment.TransactionStatus -> Text
getStatusMessage Payment.NEW = "Payment initiated"
getStatusMessage Payment.PENDING_VBV = "Awaiting 3DS authentication"
getStatusMessage Payment.CHARGED = "Payment successful"
getStatusMessage Payment.AUTHENTICATION_FAILED = "3DS authentication failed"
getStatusMessage Payment.AUTHORIZATION_FAILED = "Bank authorization failed"
getStatusMessage Payment.CANCELLED = "Payment cancelled"
getStatusMessage Payment.JUSPAY_DECLINED = "Payment declined by system"
getStatusMessage Payment.AUTHORIZING = "Authorizing payment..."
getStatusMessage Payment.COD_INITIATED = "Cash on delivery initiated"
getStatusMessage Payment.STARTED = "Payment started"
getStatusMessage Payment.AUTO_REFUNDED = "Payment auto-refunded"
getStatusMessage Payment.CLIENT_AUTH_TOKEN_EXPIRED = "Authentication token expired"

toPaymentEvent :: Payment.TransactionStatus -> ST.PaymentEvent
toPaymentEvent Payment.NEW = ST.Initiate
toPaymentEvent Payment.PENDING_VBV = ST.Authorize
toPaymentEvent Payment.CHARGED = ST.Capture
toPaymentEvent Payment.AUTHENTICATION_FAILED = ST.Fail
toPaymentEvent Payment.AUTHORIZATION_FAILED = ST.Fail
toPaymentEvent Payment.CANCELLED = ST.Cancel
toPaymentEvent Payment.JUSPAY_DECLINED = ST.Fail
toPaymentEvent Payment.AUTHORIZING = ST.Authorize
toPaymentEvent Payment.COD_INITIATED = ST.Initiate
toPaymentEvent Payment.STARTED = ST.Initiate
toPaymentEvent Payment.AUTO_REFUNDED = ST.Refund
toPaymentEvent Payment.CLIENT_AUTH_TOKEN_EXPIRED = ST.Fail
