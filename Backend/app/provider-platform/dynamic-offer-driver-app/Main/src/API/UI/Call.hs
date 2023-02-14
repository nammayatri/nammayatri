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
    DCall.GetCallStatusRes,
    DCall.MobileNumberResp,
  )
where

import qualified Domain.Action.UI.Call as DCall
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Servant

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
