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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
import qualified Kernel.External.Call.Ozonetel.Types as Ozonetel
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version (DeviceType)
import Kernel.Utils.Common
import Kernel.Utils.XML (XmlText)
import Servant
import Servant.XML
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = BackendBasedCallAPI :<|> FrontendBasedCallAPI :<|> BackendBasedDriverCallApi :<|> SDKBasedCallAPI :<|> OzonetelCampaignAPI

handler :: FlowServer API
handler = backendBasedCallHandler :<|> frontendBasedCallHandler :<|> backendBasedDriverCallHandler :<|> sdkBasedCallHandler :<|> addCampaignDataHandler

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

type SDKBasedCallAPI =
  "call"
    :> "twillio"
    :> ( "accessToken"
           :> MandatoryQueryParam "bppRideId" (Id SRide.Ride)
           :> MandatoryQueryParam "user" DCall.EntityType
           :> MandatoryQueryParam "deviceType" DeviceType
           :> Get '[JSON] Kernel.Prelude.Text
           :<|> "connectedEntityTwiml"
           :> MandatoryQueryParam "bppRideId" (Id SRide.Ride)
           :> MandatoryQueryParam "user" DCall.EntityType
           :> Get '[XML] XmlText
       )

type OzonetelCampaignAPI =
  "ozonetel"
    :> "campaign"
    :> "add"
    :> TokenAuth
    :> ReqBody '[JSON] DCall.AddOzonetelCampaignDataReq
    :> Post '[JSON] Ozonetel.OzonetelAddCampaignDataResp

frontendBasedCallHandler :: FlowServer FrontendBasedCallAPI
frontendBasedCallHandler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

backendBasedDriverCallHandler :: FlowServer BackendBasedDriverCallApi
backendBasedDriverCallHandler =
  getDriverMobileNumber

sdkBasedCallHandler :: FlowServer SDKBasedCallAPI
sdkBasedCallHandler = getCallTwillioAccessToken :<|> getCallTwillioConnectedEntityTwiml

addCampaignDataHandler :: FlowServer OzonetelCampaignAPI
addCampaignDataHandler = addCampaignData

-- | Try to initiate a call driver -> customer
initiateCallToCustomer :: Id SRide.Ride -> (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DCall.CallRes
initiateCallToCustomer rideId (personId, _, merchantOpCityId) = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.initiateCallToCustomer rideId merchantOpCityId

callStatusCallback :: DCall.CallCallbackReq -> FlowHandler DCall.CallCallbackRes
callStatusCallback = withFlowHandlerAPI . DCall.callStatusCallback

getCallStatus :: Id CallStatus -> (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DCall.GetCallStatusRes
getCallStatus callStatusId _ = withFlowHandlerAPI $ DCall.getCallStatus callStatusId

directCallStatusCallback :: Text -> ExotelCallStatus -> Maybe Text -> Maybe Int -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_ duration

getCustomerMobileNumber :: Text -> Text -> Text -> Maybe Text -> ExotelCallStatus -> Text -> FlowHandler DCall.GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus = withFlowHandlerAPI . DCall.getCustomerMobileNumber callSid callFrom_ callTo_ dtmfNumber exotelCallStatus

getDriverMobileNumber :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler DCall.CallRes
getDriverMobileNumber (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI . DCall.getDriverMobileNumber (driverId, merchantId, merchantOpCityId)

getCallTwillioAccessToken :: Id SRide.Ride -> DCall.EntityType -> DeviceType -> FlowHandler Text
getCallTwillioAccessToken rideId entity deviceType = withFlowHandlerAPI $ DCall.getCallTwillioAccessToken rideId entity deviceType

getCallTwillioConnectedEntityTwiml :: Id SRide.Ride -> DCall.EntityType -> FlowHandler XmlText
getCallTwillioConnectedEntityTwiml rideId entity = withFlowHandlerAPI $ DCall.getCallTwillioConnectedEntityTwiml rideId entity

addCampaignData :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DCall.AddOzonetelCampaignDataReq -> FlowHandler Ozonetel.OzonetelAddCampaignDataResp
addCampaignData (personId, _, merchantOpCityId) req = withFlowHandlerAPI . withPersonIdLogTag personId $ DCall.addCampaignData req merchantOpCityId
