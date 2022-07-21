module API.UI.Booking (module Reexport, API, handler) where

import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Domain.Action.UI.Booking as Reexport
  ( DriverResponse (..),
    GetRideInfoRes (..),
    NotificationStatus (..),
    RideBookingListRes (..),
    RideInfo (..),
    SetDriverAcceptanceReq (..),
    SetDriverAcceptanceRes,
  )
import qualified Domain.Action.UI.Booking as DBooking
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

type API =
  "org" :> "rideBooking"
    :> ( Capture "bookingId" (Id SRB.RideBooking)
           :> TokenAuth
           :> Post '[JSON] SRB.RideBookingAPIEntity
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> Get '[JSON] RideBookingListRes
           :<|> Capture "bookingId" (Id SRB.RideBooking)
             :> "cancel"
             :> AdminTokenAuth
             :> Get '[JSON] APISuccess
       )
    :<|> "driver"
    :> "rideBooking"
    :> Capture "bookingId" (Id SRB.RideBooking)
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

bookingStatus :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler SRB.RideBookingAPIEntity
bookingStatus bookingId _ = withFlowHandlerAPI $ DBooking.bookingStatus bookingId

bookingList :: SP.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler RideBookingListRes
bookingList person mbLimit mbOffset = withFlowHandlerAPI . DBooking.bookingList person mbLimit mbOffset

bookingCancel ::
  Id SRB.RideBooking ->
  SP.Person ->
  FlowHandler APISuccess
bookingCancel bookingId = withFlowHandlerAPI . DBooking.bookingCancel bookingId

getRideInfo :: Id SRB.RideBooking -> Id SP.Person -> FlowHandler GetRideInfoRes
getRideInfo bookingId = withFlowHandlerAPI . DBooking.getRideInfo bookingId

setDriverAcceptance :: Id SRB.RideBooking -> Id SP.Person -> SetDriverAcceptanceReq -> FlowHandler SetDriverAcceptanceRes
setDriverAcceptance bookingId personId = withFlowHandlerAPI . DBooking.setDriverAcceptance bookingId personId
