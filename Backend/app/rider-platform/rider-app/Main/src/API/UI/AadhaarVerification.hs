{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.AadhaarVerification where

import qualified Domain.Action.UI.AadhaarVerification as AV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Auth

type API =
  "verifyAadhaar"
    :> ( "generateOtp"
           :> TokenAuth
           :> ReqBody '[JSON] AadhaarVerification.AadhaarOtpReq
           :> Post '[JSON] AadhaarVerification.AadhaarVerificationResp
           :<|> "verifyOtp"
             :> TokenAuth
             :> ReqBody '[JSON] AV.VerifyAadhaarOtpReq
             :> Post '[JSON] AadhaarVerification.AadhaarOtpVerifyRes
       )

handler :: FlowServer API
handler =
  generateAadhaarOtp
    :<|> verifyAadhaarOtp

generateAadhaarOtp :: (Id DP.Person, Id DM.Merchant) -> AadhaarVerification.AadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarVerificationResp
generateAadhaarOtp (personId, _) = withFlowHandlerAPI . AV.generateAadhaarOtp False Nothing personId

verifyAadhaarOtp :: (Id DP.Person, Id DM.Merchant) -> AV.VerifyAadhaarOtpReq -> FlowHandler AadhaarVerification.AadhaarOtpVerifyRes
verifyAadhaarOtp (personId, _) = withFlowHandlerAPI . AV.verifyAadhaarOtp Nothing personId
