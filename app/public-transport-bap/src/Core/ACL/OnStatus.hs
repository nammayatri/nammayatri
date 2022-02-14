module Core.ACL.OnStatus where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Core.Spec.Common.OrderState as OrderState
import qualified Core.Spec.Common.Payment as Payment
import qualified Core.Spec.OnStatus as OnStatus
import qualified Domain.Endpoints.Beckn.OnStatus as DOnStatus
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction

mkOnStatus :: OnStatus.OnStatusMessage -> Text -> DOnStatus.OnStatusReq
mkOnStatus msg txnId = do
  let payment = msg.order.payment
      bppOrderStatus = msg.order.state
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
      paymentStatus = mkPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
      bookingStatus = mkBookingStatus bppOrderStatus
      domainReq = mkDomainOnStatusReq txnId bookingStatus bppPaymentGatewayTxnStatus paymentStatus
  domainReq

-- Do we use DBooking.NEW and DBooking.AWAITING_PAYMENT?
mkBookingStatus :: OrderState.State -> DBooking.BookingStatus
mkBookingStatus = \case
  OrderState.ACTIVE -> DBooking.CONFIRMED
  OrderState.COMPLETE -> DBooking.CONFIRMED
  OrderState.CANCELLED -> DBooking.CANCELLED

mkPaymentStatus ::
  (Payment.Status, Payment.TrStatus) ->
  DPaymentTransaction.PaymentStatus
mkPaymentStatus = \case
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_CREATED) -> DPaymentTransaction.PENDING
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_EXPIRED) -> DPaymentTransaction.FAILED
  (Payment.NOT_PAID, _) -> DPaymentTransaction.PENDING
  (Payment.PAID, Payment.CAPTURED) -> DPaymentTransaction.SUCCESS
  (Payment.PAID, Payment.REFUNDED) -> DPaymentTransaction.FAILED
  (Payment.PAID, _) -> DPaymentTransaction.PENDING

mkDomainOnStatusReq ::
  Text ->
  DBooking.BookingStatus ->
  Payment.TrStatus ->
  DPaymentTransaction.PaymentStatus ->
  DOnStatus.OnStatusReq
mkDomainOnStatusReq txnId bookingStatus transactionStatus paymentStatus =
  DOnStatus.OnStatusReq
    { bookingId = Id txnId,
      transactionStatus = show transactionStatus,
      ..
    }
