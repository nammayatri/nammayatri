module API.UI.Booking.BookingId.Types where

import Beckn.Types.Id
import Domain.Types.Booking (Booking, BookingAPIEntity)
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Capture "bookingId" (Id Booking)
    :> Get '[JSON] BookingAPIEntity
