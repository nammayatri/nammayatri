module API.UI.Booking
  ( DBooking.BookingListRes,
    API,
    handler,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Booking as DBooking
import Domain.Types.Booking (BookingAPIEntity)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

type API =
  "rideBooking"
    :> ( Capture "rideBookingId" (Id SRB.Booking)
           :> TokenAuth
           :> Post '[JSON] BookingAPIEntity
           :<|> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> QueryParam "status" SRB.BookingStatus
             :> Get '[JSON] DBooking.BookingListRes
       )

handler :: FlowServer API
handler =
  bookingStatus
    :<|> bookingList

bookingStatus :: Id SRB.Booking -> Id Person.Person -> FlowHandler BookingAPIEntity
bookingStatus bookingId = withFlowHandlerAPI . DBooking.bookingStatus bookingId

bookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> FlowHandler DBooking.BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI . DBooking.bookingList personId mbLimit mbOffset mbOnlyActive
