module API.UI.Booking.Handler where

import API.UI.Booking.BookingList.Handler as BookingList
import API.UI.Booking.Types
import App.Types

handler :: FlowServer API
handler = BookingList.handler