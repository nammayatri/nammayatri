module API.Parking.Booking.Types where

import API.Parking.Booking.BookingId.TriggerStatus.Types as BookingTriggerStatus
import API.Parking.Booking.BookingId.Types as BookingStatus
import Servant

type API =
  "booking"
    :> ( BookingStatus.API
           :<|> BookingTriggerStatus.API
       )
