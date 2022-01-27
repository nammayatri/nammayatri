module API.UI.Booking.BookingList.Types where

import Beckn.Prelude
import Domain.Types.Booking.API
import Servant
import Tools.Auth

type API =
  "list"
    :> TokenAuth
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] BookingListRes
