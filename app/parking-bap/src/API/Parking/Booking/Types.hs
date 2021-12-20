module API.Parking.Booking.Types where

import API.Parking.Booking.BookingId.TriggerStatus.Types as BookingTriggerStatus
import API.Parking.Booking.BookingId.Types as BookingStatus
import API.Parking.Booking.BookingList.Types as BookingList
import Servant

type API =
  "booking"
    :> ( BookingList.API
           :<|> BookingStatus.API
           :<|> BookingTriggerStatus.API
       )
