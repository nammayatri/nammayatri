module API.UI.Booking.BookingId.Handler where

import API.UI.Booking.BookingId.Types
import App.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Endpoints.UI.Status as DStatus
import qualified Domain.Types.Booking as DBooking
import Tools.Auth

handler :: FlowServer API
handler = status

status :: PersonId -> Id DBooking.Booking -> FlowHandler DBooking.BookingAPIEntity
status _ bookingId = withFlowHandlerAPI $ DStatus.status bookingId
