module API.Parking.Booking.Handler where

import API.Parking.Booking.BookingId.Handler as BookingStatus
import API.Parking.Booking.BookingId.TriggerStatus.Handler as BookingTriggerStatus
import API.Parking.Booking.Types
import App.Types
import Servant

handler :: FlowServer API
handler =
  BookingStatus.handler
    :<|> BookingTriggerStatus.handler
