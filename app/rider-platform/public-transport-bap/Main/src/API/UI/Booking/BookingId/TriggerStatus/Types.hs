module API.UI.Booking.BookingId.TriggerStatus.Types where

import Domain.Types.Booking
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Servant
import Tools.Auth

type API =
  TokenAuth
    :> Capture "bookingId" (Id Booking)
    :> "triggerStatusUpdate"
    :> Post '[JSON] APISuccess
