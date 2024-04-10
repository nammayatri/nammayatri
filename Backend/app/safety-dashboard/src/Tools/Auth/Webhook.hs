{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Webhook where

import Data.Singletons.TH
import qualified "lib-dashboard" Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Role as DRole
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Kernel.Utils.Servant.HeaderAuth
import Servant hiding (throwError)
import qualified Storage.Queries.Merchant as SQM
import qualified "lib-dashboard" Tools.Auth.Common as Common
import Tools.Error
import "lib-dashboard" Tools.Servant.HeaderAuth

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (SafetyWebhookAuth r :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)

-- | Performs token verification with checking api access level.
type SafetyWebhookAuth at = HeaderAuthWithPayload "token" VerifyWebhook (WebhookPayload at)

data WebhookPayload (at :: DRole.DashboardAccessType)

data VerifyWebhook

data AuthToken = AuthToken
  { merchantId :: Id DMerchant.Merchant,
    authToken :: Maybe Text
  }

instance VerificationMethod VerifyWebhook where
  type VerificationResult VerifyWebhook = AuthToken
  verificationDescription =
    "Checks whether token is registered and checks person dashboard access. \
    \If you don't have a token, use registration endpoints."

instance VerificationMethodWithPayload VerifyWebhook where
  type VerificationPayloadType VerifyWebhook = DRole.DashboardAccessType

instance
  forall (at :: DRole.DashboardAccessType).
  SingI at =>
  (VerificationPayload DRole.DashboardAccessType) (WebhookPayload at)
  where
  toPayloadType _ = fromSing (sing @at)

verifyDashboardAction ::
  (Common.AuthFlow m r, Redis.HedisFlow m r, EncFlow m r) =>
  VerificationActionWithPayload VerifyWebhook m
verifyDashboardAction = VerificationActionWithPayload verifyWebhook

verifyWebhook ::
  (Common.AuthFlow m r, Redis.HedisFlow m r, EncFlow m r) =>
  DRole.DashboardAccessType ->
  RegToken ->
  m AuthToken
verifyWebhook _ token = do
  (authToken, merchantId) <- verifyToken token
  pure AuthToken {merchantId, authToken}

verifyToken ::
  (Common.AuthFlow m r, Redis.HedisFlow m r, EncFlow m r) =>
  RegToken ->
  m (Maybe Text, Id DMerchant.Merchant)
verifyToken token = do
  key <- Common.authTokenCacheKey token
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  mbTuple <- getKeyRedis key
  case mbTuple of
    Just (authToken, merchantId) -> return (authToken, merchantId)
    Nothing -> do
      merchant <- SQM.findByAuthToken token >>= fromMaybeM (TokenNotFound token)
      decryptedMerchant <- decrypt merchant
      when (merchant.enabled == Just False) $ throwError MerchantDisabled
      let authToken = decryptedMerchant.authToken
      let merchantId = decryptedMerchant.id
      setExRedis key (authToken, merchantId) authTokenCacheExpiry
      return (authToken, merchantId)
  where
    getKeyRedis :: Redis.HedisFlow m r => Text -> m (Maybe (Maybe Text, Id DMerchant.Merchant))
    getKeyRedis = Redis.get

    setExRedis :: Redis.HedisFlow m r => Text -> (Maybe Text, Id DMerchant.Merchant) -> Int -> m ()
    setExRedis = Redis.setExp
