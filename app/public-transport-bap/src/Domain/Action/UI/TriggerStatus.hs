module Domain.Action.UI.TriggerStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking.Type as DBooking
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

data StatusRes = StatusRes
  { bookingId :: Id DBooking.Booking,
    ticketId :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl
  }

triggerStatusUpdate :: EsqDBReplicaFlow m r => Id DBooking.Booking -> m StatusRes
triggerStatusUpdate bookingId = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  ticketId <- booking.ticketId & fromMaybeM BookingBppOrderIdNotFound
  pure
    StatusRes
      { bppId = booking.bppId,
        bppUrl = booking.bppUrl,
        ..
      }
