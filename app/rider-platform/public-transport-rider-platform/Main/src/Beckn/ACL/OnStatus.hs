module Beckn.ACL.OnStatus where

import Beckn.ACL.Common.MakeStatus
import qualified Beckn.Spec.Common.Payment as Payment
import qualified Beckn.Spec.OnStatus as OnStatus
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.PaymentTransaction as DPaymentTransaction
import Kernel.Prelude
import Kernel.Types.Id

mkOnStatus :: OnStatus.OnStatusMessage -> Text -> DOnStatus.OnStatusReq
mkOnStatus msg txnId = do
  let payment = msg.order.payment
      bppOrderStatus = msg.order.state
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
      paymentStatus = mkPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
      bookingStatus = mkBookingStatus bppPaymentStatus bppOrderStatus
      domainReq = mkDomainOnStatusReq txnId bookingStatus bppPaymentGatewayTxnStatus paymentStatus
  domainReq

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
