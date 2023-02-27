{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Common.MakeStatus where

import qualified Beckn.Spec.Common.OrderState as OrderState
import qualified Beckn.Spec.Common.Payment as Payment
import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.PaymentTransaction as Domain

mkBookingStatus :: Payment.Status -> OrderState.State -> Domain.BookingStatus
mkBookingStatus st = \case
  OrderState.ACTIVE -> case st of
    Payment.NOT_PAID -> Domain.AWAITING_PAYMENT
    Payment.PAID -> Domain.CONFIRMED
  OrderState.COMPLETE -> Domain.CONFIRMED
  OrderState.CANCELLED -> Domain.CANCELLED

mkPaymentStatus ::
  (Payment.Status, Payment.TrStatus) ->
  Domain.PaymentStatus
mkPaymentStatus = \case
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_CREATED) -> Domain.PENDING
  (Payment.NOT_PAID, Payment.PAYMENT_LINK_EXPIRED) -> Domain.FAILED
  (Payment.NOT_PAID, _) -> Domain.PENDING
  (Payment.PAID, Payment.CAPTURED) -> Domain.SUCCESS
  (Payment.PAID, Payment.REFUNDED) -> Domain.FAILED
  (Payment.PAID, _) -> Domain.PENDING
