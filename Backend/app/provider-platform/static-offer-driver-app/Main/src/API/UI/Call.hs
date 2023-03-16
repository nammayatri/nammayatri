{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Call (module Reexport, API, handler) where

import Domain.Action.UI.Call as Reexport
  ( CallCallbackReq,
    CallCallbackRes,
    CallRes (..),
    GetCallStatusRes,
    GetCustomerMobileNumberResp,
  )
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Environment
import EulerHS.Prelude
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI

type BackendBasedCallAPI =
  "driver" :> "ride"
    :> ( Capture "rideId" (Id SRide.Ride)
           :> "call"
           :> ( "rider"
                  :> TokenAuth
                  :> Post '[JSON] CallRes
                  :<|> Capture "callId" (Id SCS.CallStatus)
                  :> "status"
                  :> TokenAuth
                  :> Get '[JSON] GetCallStatusRes
              )
           :<|> "call"
             :> "statusCallback"
             :> ReqBody '[JSON] CallCallbackReq
             :> Post '[JSON] CallCallbackRes
       )

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
           :> Get '[JSON] GetCustomerMobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" ExotelCallStatus
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] CallCallbackRes
       )

handler :: FlowServer API
handler =
  backendBasedCallHandler
    :<|> frontendBasedCallHandler

backendBasedCallHandler :: FlowServer BackendBasedCallAPI
backendBasedCallHandler =
  ( \rideId ->
      initiateCallToCustomer rideId
        :<|> getCallStatus
  )
    :<|> callStatusCallback

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ DCall.initiateCallToCustomer rideId

callStatusCallback :: CallCallbackReq -> FlowHandler CallCallbackRes
callStatusCallback = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id SCS.CallStatus -> Id SP.Person -> FlowHandler GetCallStatusRes
getCallStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

getCustomerMobileNumber :: Text -> Text -> Text -> ExotelCallStatus -> FlowHandler GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ callStatus_ =
  withFlowHandlerAPI $ DCall.getCustomerMobileNumber callSid callFrom_ callStatus_

directCallStatusCallback :: Text -> ExotelCallStatus -> Text -> Maybe Int -> FlowHandler CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration =
  withFlowHandlerAPI $ DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration
