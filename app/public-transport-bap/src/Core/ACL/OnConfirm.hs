module Core.ACL.OnConfirm where

import Beckn.Types.Id
import Core.Spec.OnConfirm
import Domain.Endpoints.OnConfirm
import qualified Domain.Types.Booking as Domain

mkDomainOnConfirm :: Id Domain.Booking -> OnConfirmMessage -> OnConfirmMessageD
mkDomainOnConfirm bookingId msg = do
  -- it may not be so obvious why booking id is the same as transaction id
  let ticketId = msg.order.id
      paymentGatewayTxnId = msg.order.payment.params.transaction_id
      paymentGatewayTxnStatus = msg.order.payment.params.transaction_status
      paymentUrl = msg.order.payment.uri
  OnConfirmMessageD {..}
