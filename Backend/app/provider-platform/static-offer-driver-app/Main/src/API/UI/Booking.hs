{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Booking (module Reexport, API, handler) where

import Domain.Action.UI.Booking as Reexport
  ( BookingListRes (..),
    DriverResponse (..),
    GetRideInfoRes (..),
    NotificationStatus (..),
    RideInfo (..),
    SetDriverAcceptanceReq (..),
    SetDriverAcceptanceRes,
  )
import qualified Domain.Action.UI.Booking as DBooking
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "org" :> "rideBooking"
    :> ( Capture "bookingId" (Id SRB.Booking)
           :> TokenAuth
           :> Post '[JSON] SRB.BookingAPIEntity
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> QueryParam "status" SRB.BookingStatus
             :> Get '[JSON] BookingListRes
           :<|> Capture "bookingId" (Id SRB.Booking)
             :> "cancel"
             :> AdminTokenAuth
             :> Get '[JSON] APISuccess
       )
    :<|> "driver"
    :> "rideBooking"
    :> Capture "bookingId" (Id SRB.Booking)
    :> "notification"
    :> ( "respond"
           :> TokenAuth
           :> ReqBody '[JSON] SetDriverAcceptanceReq
           :> Post '[JSON] SetDriverAcceptanceRes
           :<|> TokenAuth
           :> Get '[JSON] GetRideInfoRes
       )

handler :: FlowServer API
handler =
  ( bookingStatus
      :<|> bookingList
      :<|> bookingCancel
  )
    :<|> ( \bookingId ->
             setDriverAcceptance bookingId
               :<|> getRideInfo bookingId
         )

bookingStatus :: Id SRB.Booking -> Id SP.Person -> FlowHandler SRB.BookingAPIEntity
bookingStatus bookingId _ = withFlowHandlerAPI $ DBooking.bookingStatus bookingId

bookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> FlowHandler BookingListRes
bookingList person mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI . DBooking.bookingList person mbLimit mbOffset mbOnlyActive

bookingCancel ::
  Id SRB.Booking ->
  SP.Person ->
  FlowHandler APISuccess
bookingCancel bookingId = withFlowHandlerAPI . DBooking.bookingCancel bookingId

getRideInfo :: Id SRB.Booking -> Id SP.Person -> FlowHandler GetRideInfoRes
getRideInfo bookingId = withFlowHandlerAPI . DBooking.getRideInfo bookingId

setDriverAcceptance :: Id SRB.Booking -> Id SP.Person -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance bookingId personId = withFlowHandlerAPI . DBooking.setDriverAcceptance bookingId personId
