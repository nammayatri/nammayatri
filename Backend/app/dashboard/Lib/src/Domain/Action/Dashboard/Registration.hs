{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Registration where

import qualified Domain.Types.Merchant as DMerchant
import Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import qualified Tools.Client as Client

data LoginReq = LoginReq
  { email :: Text,
    password :: Text,
    merchantId :: ShortId DMerchant.Merchant
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { authToken :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

login ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]],
    EncFlow m r
  ) =>
  LoginReq ->
  m LoginRes
login LoginReq {..} = do
  availableServers <- asks (.dataServers)
  merchant <- QMerchant.findByShortId merchantId (Proxy @m) >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  unless (merchant.serverName `elem` (availableServers <&> (.name))) $
    throwError $ InvalidRequest "Server for this merchant is not available"
  person <- QP.findByEmailAndPassword email password (Proxy @m) >>= fromMaybeM (PersonDoesNotExist email)
  _merchantAccess <- QAccess.findByPersonIdAndMerchantId person.id merchant.id (Proxy @m) >>= fromMaybeM AccessDenied --FIXME cleanup tokens for this merchantId
  token <- generateToken person.id merchant.id
  pure $ LoginRes token "Logged in successfully"

generateToken ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m Text
generateToken personId merchantId = do
  regToken <- buildRegistrationToken personId merchantId
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByMerchantId personId merchantId
  DB.runTransaction $ do
    QR.deleteAllByPersonIdAndMerchantId @m personId merchantId
    QR.create regToken
  pure $ regToken.token

logout ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logout tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByMerchantId personId tokenInfo.merchantId
  DB.runTransaction (QR.deleteAllByPersonIdAndMerchantId @m person.id tokenInfo.merchantId)
  pure $ LogoutRes "Logged out successfully"

logoutAllMerchants ::
  forall m r.
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logoutAllMerchants tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId (Proxy @m) >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  DB.runTransaction (QR.deleteAllByPersonId @m person.id)
  pure $ LogoutRes "Logged out successfully from all servers"

buildRegistrationToken :: MonadFlow m => Id DP.Person -> Id DMerchant.Merchant -> m DR.RegistrationToken
buildRegistrationToken personId merchantId = do
  rtid <- generateGUID
  token <- generateGUID
  now <- getCurrentTime
  return $
    DR.RegistrationToken
      { id = Id rtid,
        token = token,
        personId = personId,
        merchantId = merchantId,
        createdAt = now
      }
