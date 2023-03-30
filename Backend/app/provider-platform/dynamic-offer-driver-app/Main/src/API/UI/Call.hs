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
    DCall.CallCallbackRes,
    DCall.GetCustomerMobileNumberResp,
  )
where

import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI

handler :: FlowServer API
handler = backendBasedCallHandler :<|> frontendBasedCallHandler

-------- Initiate a call (Exotel) APIs --------
type BackendBasedCallAPI =
  "driver" :> "ride"
    :> ( Capture "rideId" (Id SRide.Ride)
           :> "call"
           :> ( "customer"
                  :> TokenAuth
                  :> Post '[JSON] DCall.CallRes
                  :<|> Capture "callId" (Id SCS.CallStatus)
                    :> "status"
                    :> TokenAuth
                    :> Get '[JSON] DCall.GetCallStatusRes
              )
           :<|> "call"
             :> "statusCallback"
             :> ReqBody '[JSON] DCall.CallCallbackReq
             :> Post '[JSON] DCall.CallCallbackRes
       )

backendBasedCallHandler :: FlowServer BackendBasedCallAPI
backendBasedCallHandler =
  ( \rideId ->
      initiateCallToCustomer rideId
        :<|> getCallStatus
  )
    :<|> callStatusCallback

-------- Direct call (Exotel) APIs
type FrontendBasedCallAPI =
  "exotel"
    :> "call"
    :> ( "customer"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" ExotelCallStatus
           :> Get '[JSON] DCall.GetCustomerMobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" ExotelCallStatus
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] DCall.CallCallbackRes
       )

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

-- | Try to initiate a call driver -> customer
initiateCallToCustomer :: Id SRide.Ride -> Id Person.Person -> FlowHandler DCall.CallRes
initiateCallToCustomer rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.initiateCallToCustomer rideId

callStatusCallback :: DCall.CallCallbackReq -> FlowHandler DCall.CallCallbackRes
callStatusCallback = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id CallStatus -> Id Person -> FlowHandler DCall.GetCallStatusRes
getCallStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

directCallStatusCallback :: Text -> ExotelCallStatus -> Text -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_

getCustomerMobileNumber :: Text -> Text -> Text -> ExotelCallStatus -> FlowHandler DCall.GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ = withFlowHandlerAPI . DCall.getCustomerMobileNumber callSid callFrom_ callTo_
