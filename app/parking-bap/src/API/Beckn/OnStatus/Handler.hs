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
  let mbpayment = msg.order.payment
      orderId = msg.order.id
      status = mbpayment.status
  booking <- QBooking.findByBppOrderId orderId >>= fromMaybeM BookingNotFound
  paymentDetails <- PaymentTransactionDB.findByBookingId booking.id >>= fromMaybeM PaymentDetailsNotFound
  runTransaction $ do
    QBooking.updateStatus booking $ convertBookingStatus status
    PaymentTransactionDB.updateStatus paymentDetails.id $ convertPaymentStatus status
  where
    convertBookingStatus = \case
      PAID -> BookingStatus.CONFIRMED
      NOT_PAID -> BookingStatus.AWAITING_PAYMENT
    convertPaymentStatus = \case
      PAID -> PaymentStatus.SUCCESS
      NOT_PAID -> PaymentStatus.PENDING
