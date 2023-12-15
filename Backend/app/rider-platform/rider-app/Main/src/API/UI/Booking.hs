{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Booking
  ( DBooking.BookingListRes,
    API,
    handler,
    bookingStatus,
    bookingList,
  )
where

import qualified Domain.Action.UI.Booking as DBooking
import Domain.Types.Booking (BookingAPIEntity)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
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

bookingStatus :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler BookingAPIEntity
bookingStatus bookingId = withFlowHandlerAPI . DBooking.bookingStatus bookingId

bookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> FlowHandler DBooking.BookingListRes
bookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI . DBooking.bookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive
