module API.UI.Booking.Types where

import API.UI.Booking.BookingId.TriggerStatus.Types as BookingTriggerStatus
import API.UI.Booking.BookingId.Types as BookingStatus
import API.UI.Booking.BookingList.Types as BookingList
import Servant

type API =
  "booking"
    :> ( BookingList.API
           :<|> BookingStatus.API
           :<|> BookingTriggerStatus.API
       )
