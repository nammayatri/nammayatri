module API.UI.Booking.BookingId.Types where

import Domain.Types.Booking (Booking, BookingAPIEntity)
import Kernel.Types.Id
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Capture "bookingId" (Id Booking)
    :> Get '[JSON] BookingAPIEntity
