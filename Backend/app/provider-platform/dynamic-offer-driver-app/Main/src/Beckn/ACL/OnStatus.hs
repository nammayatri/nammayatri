{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (mkOnStatusMessage, mkOnStatusMessageV2) where

import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified BecknV2.OnDemand.Types as Spec
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

mapToBecknBookingStatus :: DBooking.BookingStatus -> OnStatus.BookingStatus
mapToBecknBookingStatus DBooking.NEW = OnStatus.NEW_BOOKING
mapToBecknBookingStatus DBooking.TRIP_ASSIGNED = OnStatus.TRIP_ASSIGNED
mapToBecknBookingStatus DBooking.COMPLETED = OnStatus.BOOKING_COMPLETED
mapToBecknBookingStatus DBooking.CANCELLED = OnStatus.BOOKING_CANCELLED

mapToBecknRideStatus :: DRide.RideStatus -> OnStatus.RideStatus
mapToBecknRideStatus DRide.NEW = OnStatus.NEW
mapToBecknRideStatus DRide.INPROGRESS = OnStatus.INPROGRESS
mapToBecknRideStatus DRide.COMPLETED = OnStatus.COMPLETED
mapToBecknRideStatus DRide.CANCELLED = OnStatus.CANCELLED

mkOnStatusMessageV2 :: DStatus.DStatusRes -> Maybe Spec.ConfirmReqMessage
mkOnStatusMessageV2 res =
  Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = tfOrder res
      }

tfOrder :: DStatus.DStatusRes -> Spec.Order
tfOrder res =
  Spec.Order
    { orderId = Just res.bookingId.getId,
      orderStatus = Just $ mapToBecknBookingStatusV2 res.bookingStatus,
      orderFulfillments = tfFulfillment res,
      orderBilling = Nothing,
      orderCancellationTerms = Nothing,
      orderItems = Nothing,
      orderPayments = Nothing,
      orderProvider = Nothing,
      orderQuote = Nothing
    }
  where
    tfFulfillment resp =
      resp.mbRide <&> \ride ->
        [ Spec.Fulfillment
            { fulfillmentId = Just ride.id.getId,
              fulfillmentType = Just $ mapToBecknRideStatusV2 ride.status,
              fulfillmentState = Nothing,
              fulfillmentAgent = Nothing,
              fulfillmentCustomer = Nothing,
              fulfillmentStops = Nothing,
              fulfillmentTags = Nothing,
              fulfillmentVehicle = Nothing
            }
        ]

mapToBecknBookingStatusV2 :: DBooking.BookingStatus -> Text
mapToBecknBookingStatusV2 DBooking.NEW = "NEW_BOOKING"
mapToBecknBookingStatusV2 DBooking.TRIP_ASSIGNED = "TRIP_ASSIGNED"
mapToBecknBookingStatusV2 DBooking.COMPLETED = "BOOKING_COMPLETED"
mapToBecknBookingStatusV2 DBooking.CANCELLED = "BOOKING_CANCELLED"

mapToBecknRideStatusV2 :: DRide.RideStatus -> Text
mapToBecknRideStatusV2 DRide.NEW = "NEW"
mapToBecknRideStatusV2 DRide.INPROGRESS = "INPROGRESS"
mapToBecknRideStatusV2 DRide.COMPLETED = "COMPLETED"
mapToBecknRideStatusV2 DRide.CANCELLED = "CANCELLED"
