module API.Parking.Booking.Handler where

import API.Parking.Booking.BookingId.Handler as BookingId
import API.Parking.Booking.Types
import App.Types

handler :: FlowServer API
handler = BookingId.handler
