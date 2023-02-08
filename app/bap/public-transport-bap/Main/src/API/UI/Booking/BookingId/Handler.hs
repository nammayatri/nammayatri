module API.UI.Booking.BookingId.Handler where

import API.UI.Booking.BookingId.Types
import qualified Domain.Action.UI.Status as DStatus
import qualified Domain.Types.Booking as DBooking
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Auth

handler :: FlowServer API
handler = status

status :: PersonId -> Id DBooking.Booking -> FlowHandler DBooking.BookingAPIEntity
status _ bookingId = withFlowHandlerAPI $ DStatus.status bookingId
