module API.UI.Call
  ( API,
    handler,
    DCall.CallCallbackRes,
    DCall.GetCallStatusRes,
    DCall.MobileNumberResp,
  )
where

import Beckn.Prelude
import qualified Domain.Action.UI.Call as DCall
import Environment
import Servant
import Utils.Common

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
           :> Get '[JSON] DCall.MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] DCall.CallCallbackRes
       )

-- :<|> "ride"
--   :> Capture "rideId" (Id DRide.Ride)
--   :> "call"
--   :> "status"
--   :> TokenAuth
--   :> Get '[JSON] CallAPI.GetCallStatusRes

handler :: FlowServer API
handler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

-- :<|> getCallStatus

directCallStatusCallback :: Text -> Text -> Text -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_

getCustomerMobileNumber :: Text -> Text -> Text -> Text -> FlowHandler DCall.MobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ = withFlowHandlerAPI .DCall.getCustomerMobileNumber callSid callFrom_

-- getCallStatus :: Id SCS.CallStatus -> FlowHandler DCall.GetCallStatusRes
-- getCallStatus = withFlowHandlerAPI . DCall.getCallStatus
