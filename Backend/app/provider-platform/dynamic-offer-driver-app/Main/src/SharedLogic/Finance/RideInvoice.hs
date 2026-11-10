module SharedLogic.Finance.RideInvoice
  ( rideFinanceInvoiceId,
    isFinalRideOfBooking,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude

rideFinanceInvoiceId :: DRide.Ride -> DRB.Booking -> Maybe Text
rideFinanceInvoiceId ride booking
  | ride.status == DRide.COMPLETED = booking.financeInvoiceId
  | ride.status == DRide.CANCELLED && booking.status == DRB.CANCELLED = booking.financeInvoiceId
  | otherwise = Nothing

isFinalRideOfBooking :: UTCTime -> [DRide.Ride] -> Bool
isFinalRideOfBooking rideCreatedAt = all (\otherRide -> otherRide.createdAt <= rideCreatedAt)
