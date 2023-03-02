{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

triggerStatusUpdate :: forall m r. EsqDBFlow m r => Id DBooking.Booking -> m StatusRes
triggerStatusUpdate bookingId = do
  booking <- QBooking.findById bookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  ticketId <- booking.ticketId & fromMaybeM BookingBppOrderIdNotFound
  pure
    StatusRes
      { bppId = booking.bppId,
        bppUrl = booking.bppUrl,
        ..
      }
