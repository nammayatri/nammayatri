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
    backendBasedDriverCallHandler,
  )
where

import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI :<|> BackendBasedDriverCallApi

handler :: FlowServer API
handler = backendBasedCallHandler :<|> frontendBasedCallHandler :<|> backendBasedDriverCallHandler

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
           :> QueryParam "digits" Text
           :> MandatoryQueryParam "CallStatus" ExotelCallStatus
           :> MandatoryQueryParam "To" Text
           :> Get '[JSON] DCall.GetCustomerMobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" ExotelCallStatus
           :> QueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> QueryParam "CallDuration" Int
           :> Get '[JSON] DCall.CallCallbackRes
       )

type BackendBasedDriverCallApi =
  "driver"
    :> "register"
    :> "call"
    :> "driver"
    :> TokenAuth
    :> MandatoryQueryParam "RC" Text
    :> Get '[JSON] DCall.CallRes

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

backendBasedDriverCallHandler :: FlowServer BackendBasedDriverCallApi
backendBasedDriverCallHandler =
  getDriverMobileNumber

-- | Try to initiate a call driver -> customer
initiateCallToCustomer :: Id SRide.Ride -> (Id Person.Person, Id DM.Merchant) -> FlowHandler DCall.CallRes
initiateCallToCustomer rideId (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.initiateCallToCustomer rideId

callStatusCallback :: DCall.CallCallbackReq -> FlowHandler DCall.CallCallbackRes
callStatusCallback = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id CallStatus -> (Id Person.Person, Id DM.Merchant) -> FlowHandler DCall.GetCallStatusRes
getCallStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

directCallStatusCallback :: Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration

getCustomerMobileNumber :: Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> FlowHandler DCall.GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus = withFlowHandlerAPI . DCall.getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus

getDriverMobileNumber :: (Id Person.Person, Id DM.Merchant) -> Text -> FlowHandler DCall.CallRes
getDriverMobileNumber (driverId, merchantId) = withFlowHandlerAPI . DCall.getDriverMobileNumber (driverId, merchantId)
