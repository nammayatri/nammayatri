module API.UI.Booking.Handler where

import API.UI.Booking.BookingId.Handler as BookingStatus
import API.UI.Booking.BookingId.TriggerStatus.Handler as BookingTriggerStatus
import API.UI.Booking.BookingList.Handler as BookingList
import API.UI.Booking.Types
import App.Types
import Servant

handler :: FlowServer API
handler =
  BookingList.handler
    :<|> BookingStatus.handler
    :<|> BookingTriggerStatus.handler
