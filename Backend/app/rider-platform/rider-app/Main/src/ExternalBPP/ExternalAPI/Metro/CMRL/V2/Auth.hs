{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Error
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant hiding (throwError)
import Tools.Error

data AuthReq = AuthReq
  { operatorNameId :: Int,
    username :: Text,
    password :: Text,
    grantType :: Text,
    merchantId :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AuthRes = AuthRes
  { access_token :: Text,
    expires_in :: Int,
    token_type :: Text,
    refresh_token :: Text,
    key_index :: Int,
    key :: Text,
    algo :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type AuthAPI =
  "api" :> "qr" :> "v1" :> "connect" :> "token"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

authTokenKey :: Text -> Text
authTokenKey merchantId = "CMRLV2Auth:Token:" <> merchantId

refreshTokenKey :: Text -> Text
refreshTokenKey merchantId = "CMRLV2Auth:RefreshToken:" <> merchantId

encryptionKeyKey :: Text -> Text
encryptionKeyKey merchantId = "CMRLV2Auth:EncryptionKey:" <> merchantId

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.get (authTokenKey config.merchantId)
  case authToken of
    Nothing -> resetAuthToken config
    Just token -> return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
resetAuthToken config = do
  password <- decrypt config.password
  auth <-
    callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.operatorNameId config.username password "password" config.merchantId) "authCMRLV2" authAPI
      >>= fromEitherM (ExternalAPICallError (Just "CMRL_V2_AUTH_API") config.networkHostUrl)
  -- Cache access token (24 hours validity as per docs)
  Hedis.setExp (authTokenKey config.merchantId) auth.access_token (auth.expires_in)
  -- Cache refresh token (7 days validity as per docs)
  Hedis.setExp (refreshTokenKey config.merchantId) auth.refresh_token (7 * 24 * 3600)
  -- Cache encryption key
  Hedis.setExp (encryptionKeyKey config.merchantId) auth.key (7 * 24 * 3600)
  return auth.access_token

callCMRLV2API ::
  ( HasCallStack,
    CoreMetrics m,
    SanitizedUrl api,
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  CMRLV2Config ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  m res
callCMRLV2API config eulerClientFunc description proxy = do
  token <- getAuthToken config
  eitherResp <- withTryCatch "CMRLV2:auth" $ callApiUnwrappingApiError (identity @CMRLV2Error) Nothing Nothing Nothing config.networkHostUrl (eulerClientFunc token) description proxy
  case eitherResp of
    Left exec -> do
      let mbError = fromException @CMRLV2Error exec
          errorCode = mbError <&> toErrorCode
      case errorCode of
        Just "UNAUTHORIZED" -> do
          void $ resetAuthToken config
          callCMRLV2API config eulerClientFunc description proxy
        _ -> do
          case mbError of
            Just err -> throwError err
            Nothing -> throwError $ InternalError "CMRL V2 API Failed"
    Right resp -> return resp
