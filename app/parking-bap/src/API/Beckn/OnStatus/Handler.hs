module API.Beckn.OnStatus.Handler where

import qualified API.Beckn.OnStatus.Types as OnStatus
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding ((<&>))
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import Core.API.Types (BecknCallbackReq)
import qualified Core.Context as Context
import Core.OnConfirm
import qualified Core.OnStatus as OnStatus
import Core.Order (OrderState (ACTIVE, CANCELLED, COMPLETE))
import qualified Domain.Booking as BookingStatus
import qualified Domain.PaymentTransaction as PaymentStatus
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnStatus.API
handler = onStatus

onStatus ::
  SignatureAuthResult ->
  BecknCallbackReq OnStatus.OnStatusMessage ->
  FlowHandler AckResponse
onStatus _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_status req" (encodeToText req)
  validateContext Context.ON_STATUS $ req.context
  case req.contents of
    Right msg -> handleOnStatus msg
    Left err -> logTagError "on_status req" $ "on_status error: " <> show err
  return Ack

handleOnStatus :: EsqDBFlow m r => OnStatus.OnStatusMessage -> m ()
handleOnStatus msg = do
  let payment = msg.order.payment
      orderId = msg.order.id
      bppOrderStatus = msg.order.state
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
  booking <- QBooking.findByBppOrderId orderId >>= fromMaybeM BookingNotFound
  paymentDetails <- PaymentTransactionDB.findByBookingId booking.id >>= fromMaybeM PaymentDetailsNotFound
  runTransaction $ do
    QBooking.updateStatus booking $ convertBookingStatus bppOrderStatus
    PaymentTransactionDB.updateStatus paymentDetails.id $ convertPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
  where
    convertBookingStatus = \case
      Just ACTIVE -> BookingStatus.CONFIRMED
      Just COMPLETE -> BookingStatus.CONFIRMED
      Just CANCELLED -> BookingStatus.CANCELLED
      Nothing -> BookingStatus.AWAITING_PAYMENT
    convertPaymentStatus = \case
      (NOT_PAID, PAYMENT_LINK_CREATED) -> PaymentStatus.PENDING
      (NOT_PAID, PAYMENT_LINK_EXPIRED) -> PaymentStatus.FAILED
      (NOT_PAID, _) -> PaymentStatus.PENDING
      (PAID, CAPTURED) -> PaymentStatus.SUCCESS
      (PAID, REFUNDED) -> PaymentStatus.FAILED
      (PAID, _) -> PaymentStatus.PENDING
