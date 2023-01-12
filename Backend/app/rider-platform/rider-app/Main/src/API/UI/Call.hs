{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI

handler :: FlowServer API
handler = backendBasedCallHandler :<|> frontendBasedCallHandler

-------- Initiate a call (Exotel) APIs --------
type BackendBasedCallAPI =
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

backendBasedCallHandler :: FlowServer BackendBasedCallAPI
backendBasedCallHandler rideId =
  initiateCallToDriver rideId
    :<|> callStatusCallback rideId
    :<|> getCallStatus rideId

type FrontendBasedCallAPI =
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

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
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
