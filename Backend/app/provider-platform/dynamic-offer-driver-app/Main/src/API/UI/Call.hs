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
import Environment
import Kernel.External.Call.Exotel.Types (ExotelCallStatus)
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
           :> MandatoryQueryParam "CallStatus" ExotelCallStatus
           :> Get '[JSON] DCall.GetCustomerMobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" ExotelCallStatus
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] DCall.CallCallbackRes
       )

handler :: FlowServer API
handler =
  getCustomerMobileNumber
    :<|> directCallStatusCallback

directCallStatusCallback :: Text -> ExotelCallStatus -> Text -> Maybe Int -> FlowHandler DCall.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ = withFlowHandlerAPI . DCall.directCallStatusCallback callSid dialCallStatus_ recordingUrl_

getCustomerMobileNumber :: Text -> Text -> Text -> ExotelCallStatus -> FlowHandler DCall.GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ = withFlowHandlerAPI . DCall.getCustomerMobileNumber callSid callFrom_
