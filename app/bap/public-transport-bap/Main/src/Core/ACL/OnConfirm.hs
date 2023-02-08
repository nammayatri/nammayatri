module Core.ACL.OnConfirm where

import Core.ACL.Common.MakeStatus (mkBookingStatus, mkPaymentStatus)
import Core.Spec.OnConfirm
import Domain.Action.Beckn.OnConfirm
import qualified Domain.Types.Booking as Domain
import Kernel.Prelude
import Kernel.Types.Id

mkDomainOnConfirm :: Id Domain.Booking -> OnConfirmMessage -> OnConfirmMessageD
mkDomainOnConfirm bookingId msg = do
  -- it may not be so obvious why booking id is the same as transaction id
  let ticketId = msg.order.id
      paymentGatewayTxnId = msg.order.payment.params.transaction_id
      paymentGatewayTxnStatus = show msg.order.payment.params.transaction_status
      paymentUrl = msg.order.payment.uri
      bppOrderStatus = msg.order.state
      payment = msg.order.payment
      bppPaymentStatus = payment.status
      bppPaymentGatewayTxnStatus = payment.params.transaction_status
      paymentStatus = mkPaymentStatus (bppPaymentStatus, bppPaymentGatewayTxnStatus)
      bookingStatus = mkBookingStatus bppPaymentStatus bppOrderStatus

  OnConfirmMessageD {..}
