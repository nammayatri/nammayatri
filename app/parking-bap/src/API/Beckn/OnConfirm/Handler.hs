module API.Beckn.OnConfirm.Handler where

import qualified API.Beckn.OnConfirm.Types as OnConfirm
import App.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult)
import qualified Core.Common.Context as Context
import qualified Core.OnConfirm as OnConfirm
import qualified Domain.Booking as DBooking
import Domain.PaymentTransaction (PaymentStatus (PENDING))
import qualified Domain.PaymentTransaction as DPaymentTransaction
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.PaymentTransaction as PaymentTransactionDB
import Tools.Context (validateContext)
import Tools.Error

handler ::
  SignatureAuthResult ->
  FlowServer OnConfirm.API
handler = onConfirm

onConfirm ::
  SignatureAuthResult ->
  BecknCallbackReq OnConfirm.OnConfirmMessage ->
  FlowHandler AckResponse
onConfirm _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  logTagDebug "on_confirm req" (encodeToText req)
  validateContext Context.ON_CONFIRM $ req.context
  case req.contents of
    Right msg -> handleOnConfirm (Id req.context.transaction_id) msg
    Left err -> logTagError "on_confirm req" $ "on_confirm error: " <> show err
  return Ack

handleOnConfirm :: EsqDBFlow m r => Id DBooking.Booking -> OnConfirm.OnConfirmMessage -> m ()
handleOnConfirm bookingId msg = do
  booking <- QBooking.findById bookingId >>= fromMaybeM BookingDoesNotExist
  now <- getCurrentTime
  let updBooking =
        booking{status = DBooking.AWAITING_PAYMENT,
                bppOrderId = Just msg.order.id,
                ticketId = Just msg.order.id,
                ticketCreatedAt = Just now
               }
  paymentData <- buildPaymentData updBooking msg
  runTransaction $ do
    QBooking.updateStatusAndBppOrderId updBooking
    PaymentTransactionDB.create paymentData

buildPaymentData :: MonadFlow m => DBooking.Booking -> OnConfirm.OnConfirmMessage -> m DPaymentTransaction.PaymentTransaction
buildPaymentData booking msg = do
  id <- generateGUID
  now <- getCurrentTime
  return
    DPaymentTransaction.PaymentTransaction
      { id = Id id,
        bookingId = booking.id,
        paymentGatewayTxnId = msg.order.payment.params.transaction_id,
        paymentGatewayTxnStatus = show msg.order.payment.params.transaction_status,
        fare = booking.fare,
        status = PENDING,
        paymentUrl = msg.order.payment.uri,
        updatedAt = now,
        createdAt = now
      }
