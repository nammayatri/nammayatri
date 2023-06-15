{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.DriverOnboarding where

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
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
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
       )
    :<|> "driver" :> "referral"
      :> TokenAuth
      :> ReqBody '[JSON] DriverOnboarding.ReferralReq
      :> Post '[JSON] DriverOnboarding.ReferralRes

handler :: FlowServer API
handler =
  ( verifyDL
      :<|> verifyRC
      :<|> statusHandler
      :<|> validateImage
      :<|> validateImageFile
  )
    :<|> addReferral

verifyDL :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.DriverDLReq -> FlowHandler DriverOnboarding.DriverDLRes
verifyDL (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.verifyDL False Nothing (personId, merchantId)

verifyRC :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.DriverRCReq -> FlowHandler DriverOnboarding.DriverRCRes
verifyRC (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.verifyRC False Nothing (personId, merchantId)

statusHandler :: (Id DP.Person, Id DM.Merchant) -> FlowHandler DriverOnboarding.StatusRes
statusHandler = withFlowHandlerAPI . DriverOnboarding.statusHandler

validateImage :: (Id DP.Person, Id DM.Merchant) -> Image.ImageValidateRequest -> FlowHandler Image.ImageValidateResponse
validateImage (personId, merchantId) = withFlowHandlerAPI . Image.validateImage False (personId, merchantId)

validateImageFile :: (Id DP.Person, Id DM.Merchant) -> Image.ImageValidateFileRequest -> FlowHandler Image.ImageValidateResponse
validateImageFile (personId, merchantId) = withFlowHandlerAPI . Image.validateImageFile False (personId, merchantId)

addReferral :: (Id DP.Person, Id DM.Merchant) -> DriverOnboarding.ReferralReq -> FlowHandler DriverOnboarding.ReferralRes
addReferral (personId, merchantId) = withFlowHandlerAPI . DriverOnboarding.addReferral (personId, merchantId)
