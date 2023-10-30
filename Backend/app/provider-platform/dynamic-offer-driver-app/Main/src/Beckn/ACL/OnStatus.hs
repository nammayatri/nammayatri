{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (mkOnStatusMessage) where

import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude

mkOnStatusMessage :: DStatus.DStatusRes -> OnStatus.OnStatusMessage
mkOnStatusMessage res =
  OnStatus.OnStatusMessage
    { order =
        OnStatus.Order
          { id = res.bookingId.getId,
            status = mapToBecknBookingStatus res.bookingStatus,
            ..
          }
    }
  where
    fulfillment =
      res.mbRide <&> \ride ->
        OnStatus.FulfillmentInfo
          { id = ride.id.getId,
            status = mapToBecknRideStatus ride.status
          }

mapToBecknBookingStatus :: DBooking.BookingStatus -> OnStatus.BookingStatus -- TODO FWB: What to do here?
mapToBecknBookingStatus DBooking.NEW = OnStatus.NEW_BOOKING
mapToBecknBookingStatus DBooking.TRIP_ASSIGNED = OnStatus.TRIP_ASSIGNED
mapToBecknBookingStatus DBooking.COMPLETED = OnStatus.BOOKING_COMPLETED
mapToBecknBookingStatus DBooking.CANCELLED = OnStatus.BOOKING_CANCELLED

mapToBecknRideStatus :: DRide.RideStatus -> OnStatus.RideStatus
mapToBecknRideStatus DRide.NEW = OnStatus.NEW
mapToBecknRideStatus DRide.INPROGRESS = OnStatus.INPROGRESS
mapToBecknRideStatus DRide.COMPLETED = OnStatus.COMPLETED
mapToBecknRideStatus DRide.CANCELLED = OnStatus.CANCELLED
