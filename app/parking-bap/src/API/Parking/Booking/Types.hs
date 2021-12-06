module API.Parking.Booking.Types where

import API.Parking.Booking.BookingId.Types as BookingId
import Servant

type API = "booking" :> BookingId.API