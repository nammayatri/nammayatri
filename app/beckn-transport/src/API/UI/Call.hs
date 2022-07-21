module API.UI.Call (module Reexport, API, handler) where

import App.Types
import Beckn.Types.Id
import Domain.Action.UI.Call as Reexport
  ( CallCallbackReq,
    CallCallbackRes,
    CallRes (..),
    GetCallStatusRes,
  )
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

type API =
  "driver" :> "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "rider"
           :> TokenAuth
           :> Post '[JSON] CallRes
           :<|> "statusCallback"
           :> ReqBody '[JSON] CallCallbackReq
           :> Post '[JSON] CallCallbackRes
           :<|> Capture "callId" (Id SCS.CallStatus)
           :> "status"
           :> TokenAuth
           :> Get '[JSON] GetCallStatusRes
       )

handler :: FlowServer API
handler rideId =
  initiateCallToCustomer rideId
    :<|> callStatusCallback rideId
    :<|> getCallStatus rideId

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ DCall.initiateCallToCustomer rideId

callStatusCallback :: Id SRide.Ride -> CallCallbackReq -> FlowHandler CallCallbackRes
callStatusCallback _ = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id SRide.Ride -> Id SCS.CallStatus -> Id SP.Person -> FlowHandler GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId
