module API.Dashboard.Booking where

import qualified "dashboard-helper-api" Dashboard.Common.Booking as Common
import qualified Domain.Action.Dashboard.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)

type API =
  "booking"
    :> Common.StuckBookingsCancelAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler =
  stuckBookingsCancel

stuckBookingsCancel :: ShortId DM.Merchant -> Common.StuckBookingsCancelReq -> FlowHandler Common.StuckBookingsCancelRes
stuckBookingsCancel merchantShortId = withFlowHandlerAPI . DBooking.stuckBookingsCancel merchantShortId
