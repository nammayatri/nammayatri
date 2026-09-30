module API.Internal.BookingLocations
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.BookingLocations as Domain
import Domain.Types.Booking (Booking)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  "booking"
    :> Capture "bookingId" (Id Booking)
    :> "locations"
    :> Header "token" Text
    :> Get '[JSON] (Maybe Domain.BookingLocationsRes)

handler :: FlowServer API
handler = getBookingLocations

getBookingLocations :: Id Booking -> Maybe Text -> FlowHandler (Maybe Domain.BookingLocationsRes)
getBookingLocations bookingId = withFlowHandlerAPI . Domain.getBookingLocations bookingId
