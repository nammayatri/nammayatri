{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.DriverOnboarding where

import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.Referral as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.Status as DriverOnboarding
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.ServantMultipart
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Auth (TokenAuth)

type API =
  "driver" :> "register"
    :> ( "dl"
           :> TokenAuth
           :> ReqBody '[JSON] DriverOnboarding.DriverDLReq
           :> Post '[JSON] DriverOnboarding.DriverDLRes
           :<|> "rc"
             :> TokenAuth
             :> ReqBody '[JSON] DriverOnboarding.DriverRCReq
             :> Post '[JSON] DriverOnboarding.DriverRCRes
           :<|> "status"
             :> TokenAuth
             :> Get '[JSON] DriverOnboarding.StatusRes
           :<|> "validateImage"
             :> TokenAuth
             :> ReqBody '[JSON] Image.ImageValidateRequest
             :> Post '[JSON] Image.ImageValidateResponse
           :<|> "validateImageFile"
             :> TokenAuth
             :> MultipartForm Tmp Image.ImageValidateFileRequest
             :> Post '[JSON] Image.ImageValidateResponse
           :<|> "generateAadhaarOtp"
             :> TokenAuth
             :> ReqBody '[JSON] AadhaarVerification.AadhaarOtpReq
             :> Post '[JSON] AadhaarVerification.AadhaarVerificationResp
           :<|> "verifyAadhaarOtp"
             :> TokenAuth
             :> ReqBody '[JSON] AV.VerifyAadhaarOtpReq
             :> Post '[JSON] AadhaarVerification.AadhaarOtpVerifyRes
           :<|> "unVerifiedAadhaarData"
             :> TokenAuth
             :> ReqBody '[JSON] AV.UnVerifiedDataReq
             :> Post '[JSON] APISuccess
       )
    :<|> "driver" :> "referral"
      :> TokenAuth
      :> ReqBody '[JSON] DriverOnboarding.ReferralReq
      :> Post '[JSON] DriverOnboarding.ReferralRes
    :<|> "rc"
      :> ( "setStatus"
             :> TokenAuth
             :> ReqBody '[JSON] DriverOnboarding.RCStatusReq
             :> Post '[JSON] APISuccess
             :<|> "delete"
               :> TokenAuth
               :> ReqBody '[JSON] DriverOnboarding.DeleteRCReq
               :> Post '[JSON] APISuccess
             :<|> "sync"
               :> TokenAuth
               :> ReqBody '[JSON] DriverOnboarding.SyncRCReq
               :> Post '[JSON] APISuccess
             :<|> "all"
               :> TokenAuth
               :> Get '[JSON] [DriverOnboarding.LinkedRC]
         )

handler :: FlowServer API
handler =
  ( verifyDL
      :<|> verifyRC
      :<|> statusHandler
      :<|> validateImage
      :<|> validateImageFile
      :<|> generateAadhaarOtp
      :<|> verifyAadhaarOtp
      :<|> unVerifiedAadhaarData
  )
    :<|> addReferral
    :<|> setRCStatus
    :<|> deleteRC
    :<|> syncRC
    :<|> getAllLinkedRCs

verifyDL :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.verifyDL False Nothing (personId, merchantId)

verifyRC :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC (personId, merchantId) req = withFlowHandlerAPI $ DriverOnboarding.verifyRC False Nothing (personId, merchantId) req Nothing

statusHandler :: (Id DP.Person, Id DM.Merchant) -> FlowHandler DriverOnboarding.StatusRes
statusHandler (personId, merchantId) = withFlowHandlerAPI $ DriverOnboarding.statusHandler (personId, merchantId) (Just True)

validateImage :: (Id DP.Person, Id DM.Merchant) -> Image.ImageValidateRequest -> FlowHandler Image.ImageValidateResponse
validateImage (personId, merchantId) = withFlowHandlerAPI . Image.validateImage False (personId, merchantId)

validateImageFile :: (Id DP.Person, Id DM.Merchant) -> Image.ImageValidateFileRequest -> FlowHandler Image.ImageValidateResponse
validateImageFile (personId, merchantId) = withFlowHandlerAPI . Image.validateImageFile False (personId, merchantId)

generateAadhaarOtp :: (Id DP.Person, Id DM.Merchant) -> AadhaarVerification.AadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp (personId, _) = withFlowHandlerAPI . AV.generateAadhaarOtp False Nothing personId

verifyAadhaarOtp :: (Id DP.Person, Id DM.Merchant) -> AV.VerifyAadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp (personId, _) = withFlowHandlerAPI . AV.verifyAadhaarOtp Nothing personId

unVerifiedAadhaarData :: (Id DP.Person, Id DM.Merchant) -> AV.UnVerifiedDataReq -> FlowHandler APISuccess
unVerifiedAadhaarData (personId, _) = withFlowHandlerAPI . AV.unVerifiedAadhaarData personId

addReferral :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.ReferralReq -> FlowHandler DriverOnboarding.ReferralRes
addReferral (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.addReferral (personId, merchantId)

setRCStatus :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.RCStatusReq -> FlowHandler APISuccess
setRCStatus (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.linkRCStatus (personId, merchantId)

deleteRC :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.DeleteRCReq -> FlowHandler APISuccess
deleteRC (personId, merchantId) req = withFlowHandlerAPI $ DriverOnboarding.deleteRC (personId, merchantId) req False

getAllLinkedRCs :: (Id DP.Person, Id DM.Merchant) -> FlowHandler [DriverOnboarding.LinkedRC]
getAllLinkedRCs (personId, merchantId) = withFlowHandlerAPI $ DriverOnboarding.getAllLinkedRCs (personId, merchantId)

syncRC :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.SyncRCReq -> FlowHandler APISuccess
syncRC _ req = withFlowHandlerAPI $ DriverOnboarding.syncRC req
