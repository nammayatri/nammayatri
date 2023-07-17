{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel (mkOnCancelMessage) where

import qualified Beckn.Types.Core.Taxi.OnCancel as OnCancel
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DCancellationReason
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude

mkOnCancelMessage :: DCancel.CancelRes -> OnCancel.OnCancelMessage
mkOnCancelMessage res =
  OnCancel.OnCancelMessage
    { order =
        OnCancel.Order
          { id = res.bookingId.getId,
            status = mapToBecknBookingStatus res.bookingStatus,
            cancellationSource = maptoBeckncancellationSource res.cancellationSource,
            ..
          }
    }
  where
    fulfillment =
      res.mbRide <&> \ride ->
        OnCancel.FulfillmentInfo
          { id = ride.id.getId,
            status = mapToBecknRideStatus ride.status
          }

mapToBecknBookingStatus :: DBooking.BookingStatus -> OnCancel.BookingStatus
mapToBecknBookingStatus DBooking.NEW = OnCancel.NEW_BOOKING
mapToBecknBookingStatus DBooking.TRIP_ASSIGNED = OnCancel.TRIP_ASSIGNED
mapToBecknBookingStatus DBooking.COMPLETED = OnCancel.BOOKING_COMPLETED
mapToBecknBookingStatus DBooking.CANCELLED = OnCancel.BOOKING_CANCELLED

mapToBecknRideStatus :: DRide.RideStatus -> OnCancel.RideStatus
mapToBecknRideStatus DRide.NEW = OnCancel.NEW
mapToBecknRideStatus DRide.INPROGRESS = OnCancel.INPROGRESS
mapToBecknRideStatus DRide.COMPLETED = OnCancel.COMPLETED
mapToBecknRideStatus DRide.CANCELLED = OnCancel.CANCELLED

maptoBeckncancellationSource :: DCancellationReason.CancellationSource -> OnCancel.CancellationSource
maptoBeckncancellationSource DCancellationReason.ByUser = OnCancel.CANCELLED_BY_USER
maptoBeckncancellationSource DCancellationReason.ByDriver = OnCancel.CANCELLED_BY_DRIVER
maptoBeckncancellationSource DCancellationReason.ByMerchant = OnCancel.CANCELLED_BY_MERCHANT
maptoBeckncancellationSource DCancellationReason.ByAllocator = OnCancel.CANCELLED_BY_ALLOCATOR
maptoBeckncancellationSource DCancellationReason.ByApplication = OnCancel.CANCELLED_BY_APPLICATION
