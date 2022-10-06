module API.UI.Booking
  ( DBooking.BookingListRes,
    API,
    handler,
  )
where

import Beckn.Types.Id
import qualified Domain.Action.UI.Booking as DBooking
import Domain.Types.Booking (BookingAPIEntity)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

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
             :> Get '[JSON] DBooking.BookingListRes
       )

handler :: FlowServer API
handler =
  bookingStatus
    :<|> bookingList

bookingStatus :: Id SRB.Booking -> Id Person.Person -> FlowHandler BookingAPIEntity
bookingStatus bookingId = withFlowHandlerAPI . DBooking.bookingStatus bookingId

bookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler DBooking.BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI . DBooking.bookingList personId mbLimit mbOffset
