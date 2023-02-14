 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
