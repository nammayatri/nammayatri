module API.UI.SharedCabBooking
  ( API,
    handler,
  )
where

import API.Types.UI.SharedCab (CancelBookingReq (..), SharedCabSession)
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Action.UI.SharedCab as DSharedCab
import qualified Domain.Action.UI.SharedCabBooking as DSharedCabBooking
import qualified Domain.Action.UI.SharedCabCall as DSharedCabCall
import qualified Domain.Types.CallStatus as SCS
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type BookingAction name =
  "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> name
    :> TokenAuth
    :> Post '[JSON] SharedCabSession

type API =
  "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] CancelBookingReq
    :> Post '[JSON] SharedCabSession
    :<|> BookingAction "boardedWithoutCode"
    :<|> BookingAction "dropped"
    :<|> BookingAction "accept"
    :<|> BookingAction "decline"
    :<|> "sharedCab"
    :> "cabFull"
    :> TokenAuth
    :> Post '[JSON] SharedCabSession
    :<|> "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> "call"
    :> TokenAuth
    :> Post '[JSON] DCall.CallRes
    :<|> "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> "message"
    :> TokenAuth
    :> ReqBody '[JSON] DSharedCabCall.SharedCabMessageReq
    :> Post '[JSON] APISuccess
    :<|> "sharedCab"
    :> "call"
    :> Capture "callId" (Id SCS.CallStatus)
    :> "status"
    :> TokenAuth
    :> Get '[JSON] DCall.GetCallStatusRes

handler :: FlowServer API
handler =
  cancelBooking
    :<|> act DSharedCabBooking.BoardedWithoutCode
    :<|> act DSharedCabBooking.Dropped
    :<|> act DSharedCabBooking.Accept
    :<|> act DSharedCabBooking.Decline
    :<|> cabFull
    :<|> callRider
    :<|> postMessage
    :<|> callStatus

cancelBooking :: Text -> DSharedCab.DriverAuthInfo -> CancelBookingReq -> FlowHandler SharedCabSession
cancelBooking bookingId auth@(personId, _, _) req =
  withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.bookingAction (DSharedCabBooking.Cancel req.reason) auth bookingId

act :: DSharedCabBooking.BookingAction -> Text -> DSharedCab.DriverAuthInfo -> FlowHandler SharedCabSession
act action bookingId auth@(personId, _, _) =
  withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.bookingAction action auth bookingId

cabFull :: DSharedCab.DriverAuthInfo -> FlowHandler SharedCabSession
cabFull auth@(personId, _, _) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.cabFull auth

callRider :: Text -> DSharedCab.DriverAuthInfo -> FlowHandler DCall.CallRes
callRider bookingId auth@(personId, _, _) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabCall.callRider auth bookingId

callStatus :: Id SCS.CallStatus -> DSharedCab.DriverAuthInfo -> FlowHandler DCall.GetCallStatusRes
callStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

postMessage :: Text -> DSharedCab.DriverAuthInfo -> DSharedCabCall.SharedCabMessageReq -> FlowHandler APISuccess
postMessage bookingId auth@(personId, _, _) req = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabCall.sendMessage auth bookingId req
