module API.Parking.Booking.BookingList.Types where

import Beckn.Prelude
import Domain.Booking.API as API
import Servant
import Tools.Auth

type API =
  "list"
    :> TokenAuth
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] API.BookingListRes
