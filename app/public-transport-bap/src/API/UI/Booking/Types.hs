module API.UI.Booking.Types where

import API.UI.Booking.BookingList.Types as BookingList
import Servant

type API =
  "booking"
    :> BookingList.API
