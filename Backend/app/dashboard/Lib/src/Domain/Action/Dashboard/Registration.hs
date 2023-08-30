{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Registration where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantAccess as DMerchantAccess
import Domain.Types.Person as DP
import qualified Domain.Types.RegistrationToken as DR
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.MerchantAccess as MA
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RegistrationToken as QR
import Tools.Auth
import qualified Tools.Auth.Common as Auth
import qualified Tools.Client as Client
import qualified Tools.Utils as Utils

data LoginReq = LoginReq
  { email :: Text,
    password :: Text,
    merchantId :: ShortId DMerchant.Merchant,
    otp :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data Enable2FAReq = Enable2FAReq
  { email :: Text,
    password :: Text,
    merchantId :: ShortId DMerchant.Merchant
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype Enable2FARes = Enable2FARes
  { qrcode :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { authToken :: Text,
    is2faMandatory :: Bool,
    is2faEnabled :: Bool,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SwitchMerchantReq = SwitchMerchantReq
  { merchantId :: ShortId DMerchant.Merchant,
    otp :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

login ::
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
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  unless (merchant.serverName `elem` (availableServers <&> (.name))) $
    throwError $ InvalidRequest "Server for this merchant is not available"
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonDoesNotExist email)
  generateLoginRes person merchant otp

switchMerchant ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]],
    EncFlow m r
  ) =>
  TokenInfo ->
  SwitchMerchantReq ->
  m LoginRes
switchMerchant authToken SwitchMerchantReq {..} = do
  availableServers <- asks (.dataServers)
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  unless (merchant.serverName `elem` (availableServers <&> (.name))) $
    throwError $ InvalidRequest "Server for this merchant is not available"
  person <- QP.findById authToken.personId >>= fromMaybeM (PersonDoesNotExist authToken.personId.getId)
  generateLoginRes person merchant otp

generateLoginRes ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    EncFlow m r
  ) =>
  DP.Person ->
  DMerchant.Merchant ->
  Maybe Text ->
  m LoginRes
generateLoginRes person merchant otp = do
  _merchantAccess <- QAccess.findByPersonIdAndMerchantId person.id merchant.id >>= fromMaybeM AccessDenied --FIXME cleanup tokens for this merchantId
  (isToken, msg) <- check2FA _merchantAccess merchant otp
  token <-
    if isToken
      then generateToken person.id merchant.id
      else pure ""
  pure $ LoginRes token merchant.is2faMandatory _merchantAccess.is2faEnabled msg

check2FA :: (EncFlow m r) => DMerchantAccess.MerchantAccess -> DMerchant.Merchant -> Maybe Text -> m (Bool, Text)
check2FA merchantAccess merchant otp =
  case (DMerchant.is2faMandatory merchant, DMerchantAccess.is2faEnabled merchantAccess) of
    (True, True) -> handle2FA merchantAccess.secretKey otp
    (True, False) -> pure (False, "2 Factor authentication is not enabled, it is mandatory for this merchant")
    _ -> pure (True, "Logged in successfully")

handle2FA ::
  ( EncFlow m r
  ) =>
  Maybe Text ->
  Maybe Text ->
  m (Bool, Text)
handle2FA secretKey otp = case (secretKey, otp) of
  (Just key, Just userOtp) -> do
    generatedOtp <- L.runIO (Utils.genTOTP key)
    if generatedOtp == read (T.unpack userOtp)
      then pure (True, "Logged in successfully")
      else pure (False, "Google Authenticator OTP does not match")
  (_, Nothing) -> pure (False, "Google Authenticator OTP is required")
  (Nothing, _) -> pure (False, "Secret key not found for 2FA")

enable2fa ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [Client.DataServer]],
    EncFlow m r
  ) =>
  Enable2FAReq ->
  m Enable2FARes
enable2fa Enable2FAReq {..} = do
  person <- QP.findByEmailAndPassword email password >>= fromMaybeM (PersonDoesNotExist email)
  merchant <- QMerchant.findByShortId merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getShortId)
  _merchantAccess <- QAccess.findByPersonIdAndMerchantId person.id merchant.id >>= fromMaybeM AccessDenied
  key <- L.runIO Utils.generateSecretKey
  Esq.runTransaction $
    MA.updatePerson2faForMerchant person.id merchant.id key
  let qrCodeUri = Utils.generateAuthenticatorURI key email merchant.shortId
  pure $ Enable2FARes qrCodeUri

generateToken ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Id DMerchant.Merchant ->
  m Text
generateToken personId merchantId = do
  findPreviousToken <- QR.findByPersonIdAndMerchantId personId merchantId
  case findPreviousToken of
    Just regToken -> pure $ regToken.token
    Nothing -> do
      regToken <- buildRegistrationToken personId merchantId
      -- this function uses tokens from db, so should be called before transaction
      Auth.cleanCachedTokensByMerchantId personId merchantId
      DB.runTransaction $ do
        QR.deleteAllByPersonIdAndMerchantId personId merchantId
        QR.create regToken
      pure $ regToken.token

logout ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logout tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokensByMerchantId personId tokenInfo.merchantId
  DB.runTransaction (QR.deleteAllByPersonIdAndMerchantId person.id tokenInfo.merchantId)
  pure $ LogoutRes "Logged out successfully"

logoutAllMerchants ::
  ( EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text]
  ) =>
  TokenInfo ->
  m LogoutRes
logoutAllMerchants tokenInfo = do
  let personId = tokenInfo.personId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- this function uses tokens from db, so should be called before transaction
  Auth.cleanCachedTokens personId
  DB.runTransaction (QR.deleteAllByPersonId person.id)
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
