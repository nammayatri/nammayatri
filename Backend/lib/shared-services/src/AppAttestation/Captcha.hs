{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Captcha
  ( verifyCaptcha,
  )
where

import AppAttestation.Domain.Types.AppAttestationConfig (FallbackConfig (..))
import AppAttestation.Domain.Types.Common (CaptchaConfig (..))
import qualified EulerHS.Types as ET
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant hiding (throwError)
import Web.FormUrlEncoded (ToForm)

data SiteVerifyReq = SiteVerifyReq
  { secret :: Text,
    response :: Text,
    remoteip :: Maybe Text
  }
  deriving (Generic, ToForm)

data SiteVerifyRes = SiteVerifyRes
  { success :: Bool,
    score :: Maybe Double,
    hostname :: Maybe Text,
    apk_package_name :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

type SiteVerifyAPI = ReqBody '[FormUrlEncoded] SiteVerifyReq :> Post '[JSON] SiteVerifyRes

siteVerifyAPI :: Proxy SiteVerifyAPI
siteVerifyAPI = Proxy

siteVerifyClient :: SiteVerifyReq -> ET.EulerClient SiteVerifyRes
siteVerifyClient = ET.client siteVerifyAPI

verifyCaptcha ::
  (MonadFlow m, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  CaptchaConfig ->
  FallbackConfig ->
  Text ->
  Maybe Text ->
  m Bool
verifyCaptcha config fallback captchaToken clientIp = do
  secret <- decrypt config.secret
  callAPI config.verifyUrl (siteVerifyClient SiteVerifyReq {response = captchaToken, remoteip = clientIp, ..}) "captchaSiteVerify" siteVerifyAPI
    <&> either (const False) accepted
  where
    accepted res =
      res.success
        && maybe True (\minimum' -> maybe False (>= minimum') res.score) fallback.captchaMinScore
        && maybe True (\allowed -> any (`elem` allowed) (catMaybes [res.hostname, res.apk_package_name])) fallback.captchaAllowedHostnames
