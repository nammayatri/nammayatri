module API.UI.Booking (module Reexport, API, handler) where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
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

bookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler BookingListRes
bookingList person mbLimit mbOffset = withFlowHandlerAPI . DBooking.bookingList person mbLimit mbOffset

bookingCancel ::
  Id SRB.Booking ->
  SP.Person ->
  FlowHandler APISuccess
bookingCancel bookingId = withFlowHandlerAPI . DBooking.bookingCancel bookingId

getRideInfo :: Id SRB.Booking -> Id SP.Person -> FlowHandler GetRideInfoRes
getRideInfo bookingId = withFlowHandlerAPI . DBooking.getRideInfo bookingId

setDriverAcceptance :: Id SRB.Booking -> Id SP.Person -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance bookingId personId = withFlowHandlerAPI . DBooking.setDriverAcceptance bookingId personId
