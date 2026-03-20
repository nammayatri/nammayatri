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
toPaymentState Payment.NEW = ST.PENDING
toPaymentState Payment.PENDING_VBV = ST.PENDING
toPaymentState Payment.CHARGED = ST.CAPTURED
toPaymentState Payment.AUTHENTICATION_FAILED = ST.FAILED
toPaymentState Payment.AUTHORIZATION_FAILED = ST.FAILED
toPaymentState Payment.CANCELLED = ST.CANCELLED
toPaymentState Payment.JUSPAY_DECLINED = ST.FAILED
toPaymentState Payment.AUTHORIZING = ST.PROCESSING
toPaymentState Payment.COD_INITIATED = ST.CASH_PENDING
toPaymentState Payment.STARTED = ST.INITIATED
toPaymentState Payment.AUTO_REFUNDED = ST.REFUNDED
toPaymentState Payment.CLIENT_AUTH_TOKEN_EXPIRED = ST.FAILED

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
toPaymentEvent Payment.NEW = ST.INITIATE
toPaymentEvent Payment.PENDING_VBV = ST.AUTHORIZE
toPaymentEvent Payment.CHARGED = ST.CAPTURE
toPaymentEvent Payment.AUTHENTICATION_FAILED = ST.FAIL
toPaymentEvent Payment.AUTHORIZATION_FAILED = ST.FAIL
toPaymentEvent Payment.CANCELLED = ST.CANCEL
toPaymentEvent Payment.JUSPAY_DECLINED = ST.FAIL
toPaymentEvent Payment.AUTHORIZING = ST.AUTHORIZE
toPaymentEvent Payment.COD_INITIATED = ST.INITIATE
toPaymentEvent Payment.STARTED = ST.INITIATE
toPaymentEvent Payment.AUTO_REFUNDED = ST.REFUND
toPaymentEvent Payment.CLIENT_AUTH_TOKEN_EXPIRED = ST.FAIL
