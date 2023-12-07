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
import qualified Domain.Types.Merchant.MerchantOperatingCity as DM
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
    :<|> "driver" :> "referral" :> "getReferredDrivers"
      :> TokenAuth
      :> Get '[JSON] DriverOnboarding.GetReferredDriverRes
    :<|> "rc"
      :> ( "setStatus"
             :> TokenAuth
             :> ReqBody '[JSON] DriverOnboarding.RCStatusReq
             :> Post '[JSON] APISuccess
             :<|> "delete"
               :> TokenAuth
               :> ReqBody '[JSON] DriverOnboarding.DeleteRCReq
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
    :<|> getReferredDrivers
    :<|> setRCStatus
    :<|> deleteRC
    :<|> getAllLinkedRCs

verifyDL :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . DriverOnboarding.verifyDL False Nothing (personId, merchantId, merchantOpCityId)

verifyRC :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC (personId, merchantId, merchantOpCityId) req = withFlowHandlerAPI $ DriverOnboarding.verifyRC False Nothing (personId, merchantId, merchantOpCityId) req Nothing

statusHandler :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> FlowHandler DriverOnboarding.StatusRes
statusHandler (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DriverOnboarding.statusHandler (personId, merchantId, merchantOpCityId) (Just True)

validateImage :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> Image.ImageValidateRequest -> FlowHandler Image.ImageValidateResponse
validateImage (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Image.validateImage False (personId, merchantId, merchantOpCityId)

validateImageFile :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> Image.ImageValidateFileRequest -> FlowHandler Image.ImageValidateResponse
validateImageFile (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . Image.validateImageFile False (personId, merchantId, merchantOpCityId)

generateAadhaarOtp :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> AadhaarVerification.AadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp (personId, _, merchantOpCityId) = withFlowHandlerAPI . AV.generateAadhaarOtp False Nothing personId merchantOpCityId

verifyAadhaarOtp :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> AV.VerifyAadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp (personId, _, merchantOpCityId) = withFlowHandlerAPI . AV.verifyAadhaarOtp Nothing personId merchantOpCityId

unVerifiedAadhaarData :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> AV.UnVerifiedDataReq -> FlowHandler APISuccess
unVerifiedAadhaarData (personId, _, _) = withFlowHandlerAPI . AV.unVerifiedAadhaarData personId

addReferral :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> DriverOnboarding.ReferralReq -> FlowHandler DriverOnboarding.ReferralRes
addReferral (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . DriverOnboarding.addReferral (personId, merchantId, merchantOpCityId)

getReferredDrivers :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> FlowHandler DriverOnboarding.GetReferredDriverRes
getReferredDrivers (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DriverOnboarding.getReferredDrivers (personId, merchantId, merchantOpCityId)

setRCStatus :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> DriverOnboarding.RCStatusReq -> FlowHandler APISuccess
setRCStatus (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI . DriverOnboarding.linkRCStatus (personId, merchantId, merchantOpCityId)

deleteRC :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> DriverOnboarding.DeleteRCReq -> FlowHandler APISuccess
deleteRC (personId, merchantId, merchantOpCityId) req = withFlowHandlerAPI $ DriverOnboarding.deleteRC (personId, merchantId, merchantOpCityId) req False

getAllLinkedRCs :: (Id DP.Person, Id DM.Merchant, Id DM.MerchantOperatingCity) -> FlowHandler [DriverOnboarding.LinkedRC]
getAllLinkedRCs (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DriverOnboarding.getAllLinkedRCs (personId, merchantId, merchantOpCityId)
