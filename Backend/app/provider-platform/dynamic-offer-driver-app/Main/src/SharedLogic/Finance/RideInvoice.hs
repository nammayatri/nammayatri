module SharedLogic.Finance.RideInvoice
  ( rideFinanceInvoiceId,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude

-- | The finance invoice to show for a ride. The invoice is booking-level, so it is shown
--   only on the ride the booking ended with — a completed ride, or the cancellation that
--   ended the booking — and not on a ride that was cancelled before the booking was
--   reassigned to another driver.
rideFinanceInvoiceId :: DRide.Ride -> DRB.Booking -> Maybe Text
rideFinanceInvoiceId ride booking
  | ride.status == DRide.COMPLETED = booking.financeInvoiceId
  | ride.status == DRide.CANCELLED && booking.status == DRB.CANCELLED = booking.financeInvoiceId
  | otherwise = Nothing
