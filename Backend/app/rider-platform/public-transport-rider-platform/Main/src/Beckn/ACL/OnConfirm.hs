 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnConfirm where

import Beckn.ACL.Common.MakeStatus (mkBookingStatus, mkPaymentStatus)
import Beckn.Spec.OnConfirm
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
