{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (threadDelay)
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.V2.Error
import Kernel.External.Encryption
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder, runThroughMasterCloud)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (fromResponse)
import Kernel.Utils.Common
import qualified Network.HTTP.Types as HTTP
import Servant hiding (throwError)
import Servant.Client (ClientError (FailureResponse))
import qualified Servant.Client as SC
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

refreshLockKey :: Text -> Text
refreshLockKey merchantId = "CMRLV2Auth:RefreshLock:" <> merchantId

maxLockWaitRetries :: Int
maxLockWaitRetries = 5

maxAuthRetries :: Int
maxAuthRetries = 5

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> m Text
getAuthToken config = getAuthTokenWithRetries config maxLockWaitRetries

getAuthTokenWithRetries :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> Int -> m Text
getAuthTokenWithRetries config retriesLeft = do
  logDebug $ "[CMRLV2:Auth] Checking for cached auth token for merchantId: " <> config.merchantId
  authToken :: (Maybe Text) <- Hedis.get (authTokenKey config.merchantId)
  case authToken of
    Nothing -> do
      logDebug "[CMRLV2:Auth] No cached token found, requesting new token"
      resetAuthToken config retriesLeft
    Just token -> do
      logDebug "[CMRLV2:Auth] Using cached auth token"
      return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLV2Config -> Int -> m Text
resetAuthToken config retriesLeft = do
  let lockKey = refreshLockKey config.merchantId
  lockAcquired <- Hedis.tryLockRedis lockKey 30
  if lockAcquired
    then do
      logInfo $ "[CMRLV2:Auth] Acquired Redis lock for token refresh for merchantId: " <> config.merchantId
      let unlockLock = do
            logInfo $ "[CMRLV2:Auth] Releasing Redis lock for merchantId: " <> config.merchantId
            Hedis.unlockRedis lockKey

      auth <-
        ( do
            -- Double check cache after acquiring lock
            mbToken <- Hedis.get (authTokenKey config.merchantId)
            case mbToken of
              Just token -> do
                logInfo $ "[CMRLV2:Auth] Token found in cache after acquiring lock for merchantId: " <> config.merchantId
                return token
              Nothing -> do
                logInfo $ "[CMRLV2:Auth] Requesting new auth token from: " <> showBaseUrl config.networkHostUrl
                password <- decrypt config.password
                let authReq = AuthReq config.operatorNameId config.username password "password" config.merchantId
                authRes <-
                  runThroughMasterCloud config.networkHostUrl (ET.client authAPI authReq) "authCMRLV2"
                    >>= fromEitherM (ExternalAPICallError (Just "CMRL_V2_AUTH_API") config.networkHostUrl)
                logInfo $ "[CMRLV2:Auth] Successfully obtained auth token, expires_in: " <> show authRes.expires_in <> "s, key_index: " <> show authRes.key_index
                let tokenExpiry = authRes.expires_in * 90 `div` 100
                Hedis.setExp (authTokenKey config.merchantId) authRes.access_token tokenExpiry
                Hedis.setExp (refreshTokenKey config.merchantId) authRes.refresh_token (7 * 24 * 3600)
                Hedis.setExp (encryptionKeyKey config.merchantId) authRes.key (7 * 24 * 3600)
                return authRes.access_token
          )
          `finally` unlockLock
      return auth
    else do
      if retriesLeft <= 0
        then do
          logError $ "[CMRLV2:Auth] Exceeded max lock wait retries for merchantId: " <> config.merchantId
          throwError $ InternalError "CMRL V2 Auth token refresh failed after max retries"
        else do
          logInfo $ "[CMRLV2:Auth] Redis lock already held by another pod, waiting 2 seconds (retries left: " <> show retriesLeft <> ")"
          threadDelay 2000000
          getAuthTokenWithRetries config (retriesLeft - 1)

callCMRLV2API ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    HasMasterCloudForwarder r
  ) =>
  CMRLV2Config ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  m res
callCMRLV2API config eulerClientFunc description _proxy = callCMRLV2APIWithRetries config eulerClientFunc description _proxy maxAuthRetries

callCMRLV2APIWithRetries ::
  ( HasCallStack,
    CoreMetrics m,
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    HasMasterCloudForwarder r
  ) =>
  CMRLV2Config ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  Int ->
  m res
callCMRLV2APIWithRetries config eulerClientFunc description _proxy authRetriesLeft = do
  logInfo $ "[CMRLV2:API] Calling API: " <> description <> " at " <> showBaseUrl config.networkHostUrl
  token <- getAuthToken config
  result <- runThroughMasterCloud config.networkHostUrl (eulerClientFunc token) description
  case result of
    Right resp -> do
      logInfo $ "[CMRLV2:API] API call successful: " <> description
      return resp
    Left clientErr -> do
      case clientErr of
        FailureResponse _req resp -> do
          let code = HTTP.statusCode (SC.responseStatusCode resp)
          logError $ "[CMRLV2:API] API call failed: " <> description <> ", statusCode: " <> show code
          if code == 401
            then do
              if authRetriesLeft <= 0
                then do
                  logError "[CMRLV2:API] Token expired, max auth retries exceeded"
                  throwError $ InternalError "CMRL V2 API auth failed after max retries"
                else do
                  logError $ "[CMRLV2:API] Token expired, refreshing and retrying... (retries left: " <> show authRetriesLeft <> ")"
                  void $ resetAuthToken config maxLockWaitRetries
                  callCMRLV2APIWithRetries config eulerClientFunc description _proxy (authRetriesLeft - 1)
            else case fromResponse @CMRLV2Error resp of
              Just cmrlErr -> throwError cmrlErr
              Nothing -> throwError $ InternalError "CMRL V2 API Failed"
        _ -> do
          logError $ "[CMRLV2:API] API call failed: " <> description <> ", error: " <> show clientErr
          throwError $ InternalError "CMRL V2 API Failed"
