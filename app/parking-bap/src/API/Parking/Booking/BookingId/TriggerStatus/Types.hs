module API.Parking.Booking.BookingId.TriggerStatus.Types where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Domain.Booking
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Capture "bookingId" (Id Booking)
    :> "triggerStatusUpdate"
    :> Post '[JSON] APISuccess
