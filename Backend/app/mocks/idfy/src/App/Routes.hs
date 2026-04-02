{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes where

import App.Types (AppEnv, MockDocConfig)
import EulerHS.Prelude
import Kernel.External.Verification.Idfy.Client
import Kernel.External.Verification.Idfy.Config (AccountId, ApiKey)
import qualified Kernel.External.Verification.Idfy.Types.Request as Idfy
import qualified Kernel.External.Verification.Idfy.Types.Response as Idfy
import Kernel.Types.App (FlowServerR)
import qualified Product.Idfy as P
import Servant hiding (throwError)

-- PAN and GST verify API types (not exported from Client, defined locally)
type VerifyPanAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_pan"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] Idfy.PanVerificationRequest
    :> Post '[JSON] Idfy.IdfySuccess

type VerifyGstAPI =
  "v3" :> "tasks" :> "async" :> "verify_with_source" :> "ind_gst_certificate"
    :> Header "api-key" ApiKey
    :> Header "account-id" AccountId
    :> ReqBody '[JSON] Idfy.GstVerificationRequest
    :> Post '[JSON] Idfy.IdfySuccess

type ConfigureMockAPI =
  "configure" :> ReqBody '[JSON] MockDocConfig :> Post '[JSON] MockDocConfig

type MockIdfyAPI =
  Get '[JSON] Text
    :<|> VerifyDLAPI
    :<|> VerifyRCAPI
    :<|> VerifyPanAPI
    :<|> VerifyGstAPI
    :<|> VerifyPanAadhaarLinkAPI
    :<|> ValidateImage
    :<|> ExtractDLImage
    :<|> ExtractRCAPI
    :<|> ExtractPanImage
    :<|> ExtractGSTImage
    :<|> ExtractAadhaarImage
    :<|> NameCompareAPI
    :<|> ConfigureMockAPI

mockIdfyAPI :: Proxy MockIdfyAPI
mockIdfyAPI = Proxy

mockIdfyServer :: FlowServerR AppEnv MockIdfyAPI
mockIdfyServer =
  pure "MockIdfy is UP"
    :<|> P.verifyDL
    :<|> P.verifyRC
    :<|> P.verifyPan
    :<|> P.verifyGst
    :<|> P.verifyPanAadhaarLink
    :<|> P.validateImage
    :<|> P.extractDLImage
    :<|> P.extractRCImage
    :<|> P.extractPanImage
    :<|> P.extractGSTImage
    :<|> P.extractAadhaarImage
    :<|> P.nameCompare
    :<|> P.configureMock
