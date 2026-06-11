{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.CrossCloudProxy (crossCloudProxy) where

import qualified API.UI as UI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import qualified Network.HTTP.Client as Http
import qualified Network.Wai as Wai
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.RegistrationToken as QRT
import Tools.Auth (authTokenCacheKey, merchantIdFallback)
import qualified "utils" Utils.Common.CrossCloudProxy as CCP

crossCloudProxy :: Http.Manager -> Env -> Wai.Middleware
crossCloudProxy manager env =
  case env.appEnv.cloudType of
    Just podCloud | podCloud /= UNAVAILABLE -> CCP.crossCloudProxyMiddleware env.flowRuntime env.appEnv podCloud [UI.uiApiPrefix] manager resolveOwner
    _ -> identity

resolveOwner :: Text -> Flow (Maybe (CloudType, BaseUrl))
resolveOwner token = do
  mbAuthCached :: Maybe (Id DP.Person, Id DM.Merchant) <- Redis.safeGet (authTokenCacheKey token)
  mbMerchantId <- case mbAuthCached of
    Just (_, merchantId) -> pure (Just merchantId)
    Nothing -> fmap (merchantIdFallback . Id @DM.Merchant . (.merchantId)) <$> QRT.findByToken token
  case mbMerchantId of
    Nothing -> pure Nothing
    Just merchantId -> IM.withInMemCache ["crossCloudProxy:merchantCloud", merchantId.getId] 60 $ do
      mbMerchant :: Maybe DM.Merchant <- CQM.findById merchantId
      pure $ mbMerchant >>= \merchant -> (,) <$> merchant.cloudType <*> merchant.cloudBaseUrl
