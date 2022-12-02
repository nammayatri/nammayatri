module Core.ACL.Common.MakeStatus where

import qualified Core.Spec.Common.OrderState as OrderState
import qualified Core.Spec.Common.Payment as Payment
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
