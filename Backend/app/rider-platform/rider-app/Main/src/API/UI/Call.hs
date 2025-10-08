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
    DCall.CallAttachments (..),
    DCall.CallCallbackRes,
    DCall.GetDriverMobileNumberResp,
    DCall.GetCallStatusRes,
  )
where

import qualified Domain.Action.UI.Call as DCall
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType (..))
import Kernel.Utils.Common
import Kernel.Utils.XML (XmlText)
import Servant
import Servant.XML
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI :<|> SDKBasedCallAPI

handler :: FlowServer API
handler = backendBasedCallHandler :<|> frontendBasedCallHandler :<|> sdkBasedCallHandler

-------- Initiate a call (Exotel) APIs --------
type BackendBasedCallAPI =
  "ride"
    :> ( Capture "rideId" (Id SRide.Ride)
           :> "call"
           :> ( "driver"
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
      initiateCallToDriver rideId
        :<|> getCallStatus
  )
    :<|> callStatusCallback

type FrontendBasedCallAPI =
  "exotel"
    :> "call"
    :> ( "driver"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> QueryParam "digits" Text
           :> MandatoryQueryParam "CallStatus" ExotelCallStatus
           :> MandatoryQueryParam "To" Text
           :> Get '[JSON] DCall.GetDriverMobileNumberResp
           :<|> "statusCallback"
             :> MandatoryQueryParam "CallSid" Text
             :> MandatoryQueryParam "DialCallStatus" ExotelCallStatus
             :> QueryParam "RecordingUrl" Text
             :> QueryParam "Legs[0][OnCallDuration]" Int
             :> QueryParam "CallDuration" Int
             :> Get '[JSON] DCall.CallCallbackRes
           :<|> "alternate"
             :> "driver"
             :> "number"
             :> MandatoryQueryParam "CallSid" Text
             :> MandatoryQueryParam "CallFrom" Text
             :> MandatoryQueryParam "CallTo" Text
             :> QueryParam "digits" Text
             :> MandatoryQueryParam "CallStatus" ExotelCallStatus
             :> MandatoryQueryParam "To" Text
             :> Get '[JSON] DCall.GetDriverMobileNumberResp
       )

type SDKBasedCallAPI =
  "call"
    :> "twillio"
    :> ( "accessToken"
           :> MandatoryQueryParam "bapRideId" (Id SRide.Ride)
           :> MandatoryQueryParam "user" DCall.EntityType
           :> MandatoryQueryParam "deviceType" DeviceType
           :> Get '[JSON] Kernel.Prelude.Text
           :<|> "connectedEntityTwiml"
           :> MandatoryQueryParam "bapRideId" (Id SRide.Ride)
           :> MandatoryQueryParam "user" DCall.EntityType
           :> Get '[XML] XmlText
       )

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
  getDriverMobileNumber
    :<|> directCallStatusCallback
    :<|> getDriverAlternateNumber

-- | Try to initiate a call customer -> driver
initiateCallToDriver :: Id SRide.Ride -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DCall.CallRes
initiateCallToDriver rideId (personId, _) = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.initiateCallToDriver rideId

callStatusCallback :: DCall.CallCallbackReq -> FlowHandler DCall.CallCallbackRes
callStatusCallback = withFlowHandlerAPI . DCall.callStatusCallback

directCallStatusCallback :: Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration

getDriverMobileNumber :: Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> FlowHandler DCall.GetDriverMobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus = withFlowHandlerAPI . DCall.getDriverMobileNumber DCall.BothNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus

getDriverAlternateNumber :: Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> FlowHandler DCall.GetDriverMobileNumberResp
getDriverAlternateNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus = withFlowHandlerAPI . DCall.getDriverMobileNumber DCall.AlternateNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus

getCallStatus :: Id CallStatus -> (Id Person.Person, Id Merchant.Merchant) -> FlowHandler DCall.GetCallStatusRes
getCallStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

sdkBasedCallHandler :: FlowServer SDKBasedCallAPI
sdkBasedCallHandler = getCallTwillioAccessToken :<|> getCallTwillioConnectedEntityTwiml

getCallTwillioAccessToken :: Id SRide.Ride -> DCall.EntityType -> DeviceType -> FlowHandler Text
getCallTwillioAccessToken rideId entity deviceType = withFlowHandlerAPI $ DCall.getCallTwillioAccessToken rideId entity deviceType

getCallTwillioConnectedEntityTwiml :: Id SRide.Ride -> DCall.EntityType -> FlowHandler XmlText
getCallTwillioConnectedEntityTwiml rideId entity = withFlowHandlerAPI $ DCall.getCallTwillioConnectedEntityTwiml rideId entity
