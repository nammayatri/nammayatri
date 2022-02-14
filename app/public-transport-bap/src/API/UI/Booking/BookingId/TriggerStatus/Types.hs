module API.UI.Booking.BookingId.TriggerStatus.Types where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Domain.Types.Booking
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Capture "bookingId" (Id Booking)
    :> "triggerStatusUpdate"
    :> Post '[JSON] APISuccess
