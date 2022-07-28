module API.UI.Call (module Reexport, DeprecatedAPI, API, handler, deprecatedHandler) where

import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Call as Reexport
  ( CallCallbackReq,
    CallCallbackRes,
    CallRes (..),
    GetCallStatusRes,
    MobileNumberResp,
  )
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude
import Servant
import Tools.Auth

type DeprecatedAPI =
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

-------- Direct call (Exotel) APIs
type API =
  "exotel"
    :> "call"
    :> ( "customer"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" Text
           :> Get '[JSON] MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] CallCallbackRes
       )

deprecatedHandler :: FlowServer DeprecatedAPI
deprecatedHandler rideId =
  initiateCallToCustomer rideId
    :<|> callStatusCallback rideId
    :<|> getCallStatus rideId

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ DCall.initiateCallToCustomer rideId

callStatusCallback :: Id SRide.Ride -> CallCallbackReq -> FlowHandler CallCallbackRes
callStatusCallback _ = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id SRide.Ride -> Id SCS.CallStatus -> Id SP.Person -> FlowHandler GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

handler :: FlowServer API
handler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

getCustomerMobileNumber :: Text -> Text -> Text -> Text -> FlowHandler MobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo callStatus_ =
  withFlowHandlerAPI $ DCall.getCustomerMobileNumber callSid callFrom_ callTo callStatus_

directCallStatusCallback :: Text -> Text -> Text -> Maybe Int -> FlowHandler CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration =
  withFlowHandlerAPI $ DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration
