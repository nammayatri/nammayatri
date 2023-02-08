module Domain.Action.UI.TriggerStatus where

import qualified Domain.Types.Booking.Type as DBooking
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

data StatusRes = StatusRes
  { bookingId :: Id DBooking.Booking,
    ticketId :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl
  }

triggerStatusUpdate :: EsqDBFlow m r => Id DBooking.Booking -> m StatusRes
triggerStatusUpdate bookingId = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  ticketId <- booking.ticketId & fromMaybeM BookingBppOrderIdNotFound
  pure
    StatusRes
      { bppId = booking.bppId,
        bppUrl = booking.bppUrl,
        ..
      }
