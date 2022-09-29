module API.UI.Call
  ( API,
    handler,
    DCall.CallRes (..),
    DCall.CallCallbackReq,
    DCall.CallCallbackRes,
    DCall.MobileNumberResp,
    DCall.GetCallStatusRes,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Servant
import Utils.Auth
import Utils.Common

type API =
  DeprecatedCallAPIs
    :<|> CallAPIs

handler :: FlowServer API
handler = deprecatedCallHandler :<|> callHandler

-------- Initiate a call (Exotel) APIs --------
type DeprecatedCallAPIs =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "driver"
           :> TokenAuth
           :> Post '[JSON] DCall.CallRes
           :<|> "statusCallback"
           :> ReqBody '[JSON] DCall.CallCallbackReq
           :> Post '[JSON] DCall.CallCallbackRes
           :<|> Capture "callId" (Id SCS.CallStatus)
           :> "status"
           :> TokenAuth
           :> Get '[JSON] DCall.GetCallStatusRes
       )

deprecatedCallHandler :: FlowServer DeprecatedCallAPIs
deprecatedCallHandler rideId =
  initiateCallToDriver rideId
    :<|> callStatusCallback rideId
    :<|> getCallStatus rideId

type CallAPIs =
  "exotel"
    :> "call"
    :> ( "driver"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" Text
           :> Get '[JSON] DCall.MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] DCall.CallCallbackRes
       )

callHandler :: FlowServer CallAPIs
callHandler =
  getDriverMobileNumber
    :<|> directCallStatusCallback

-- | Try to initiate a call customer -> driver
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler DCall.CallRes
initiateCallToDriver rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.initiateCallToDriver rideId

callStatusCallback :: Id SRide.Ride -> DCall.CallCallbackReq -> FlowHandler DCall.CallCallbackRes
callStatusCallback _ = withFlowHandlerAPI . DCall.callStatusCallback

directCallStatusCallback :: Text -> Text -> Text -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_

getDriverMobileNumber :: Text -> Text -> Text -> Text -> FlowHandler DCall.MobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ = withFlowHandlerAPI . DCall.getDriverMobileNumber callSid callFrom_ callTo_

getCallStatus :: Id SRide.Ride -> Id CallStatus -> Id Person -> FlowHandler DCall.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId
