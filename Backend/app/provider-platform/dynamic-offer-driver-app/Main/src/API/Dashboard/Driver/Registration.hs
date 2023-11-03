{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Driver.Registration where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.Dashboard.Driver.Registration as DReg
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  Common.DocumentsListAPI
    :<|> Common.GetDocumentAPI
    :<|> Common.UploadDocumentAPI
    :<|> Common.RegisterDLAPI
    :<|> Common.RegisterRCAPI
    :<|> Common.GenerateAadhaarOtpAPI
    :<|> Common.VerifyAadhaarOtpAPI
    :<|> AuthAPI
    :<|> VerifyAPI

type AuthAPI =
  Capture "mbFleet" Bool
    :> Capture "fleetOwnerId" Text
    :> "auth"
    :> ReqBody '[JSON] Common.AuthReq
    :> Post '[JSON] Common.AuthRes

type VerifyAPI =
  Capture "authId" Text
    :> Capture "mbFleet" Bool
    :> Capture "fleetOwnerId" Text
    :> "verify"
    :> ReqBody '[JSON] Common.AuthVerifyReq
    :> Post '[JSON] APISuccess

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city =
  documentsList merchantId city
    :<|> getDocument merchantId city
    :<|> uploadDocument merchantId city
    :<|> registerDL merchantId city
    :<|> registerRC merchantId city
    :<|> generateAadhaarOtp merchantId city
    :<|> verifyAadhaarOtp merchantId city
    :<|> auth merchantId city
    :<|> verify

documentsList :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList merchantShortId opCity = withFlowHandlerAPI . DReg.documentsList merchantShortId opCity

getDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument merchantShortId opCity = withFlowHandlerAPI . DReg.getDocument merchantShortId opCity

uploadDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument merchantShortId opCity driverId_ = withFlowHandlerAPI . DReg.uploadDocument merchantShortId opCity driverId_

registerDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL merchantShortId opCity driverId_ = withFlowHandlerAPI . DReg.registerDL merchantShortId opCity driverId_

registerRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC merchantShortId opCity driverId_ = withFlowHandlerAPI . DReg.registerRC merchantShortId opCity driverId_

generateAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> FlowHandler Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId opCity driverId_ = withFlowHandlerAPI . DReg.generateAadhaarOtp merchantShortId opCity driverId_

verifyAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> FlowHandler Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId opCity driverId_ = withFlowHandlerAPI . DReg.verifyAadhaarOtp merchantShortId opCity driverId_

auth :: ShortId DM.Merchant -> Context.City -> Bool -> Text -> Common.AuthReq -> FlowHandler Common.AuthRes
auth merchantShortId opCity mbFleet fleetOwnerId req = withFlowHandlerAPI $ DReg.auth merchantShortId opCity mbFleet fleetOwnerId req

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> FlowHandler APISuccess
verify authId mbFleet fleetOwnerId req = withFlowHandlerAPI $ DReg.verify authId mbFleet fleetOwnerId req
