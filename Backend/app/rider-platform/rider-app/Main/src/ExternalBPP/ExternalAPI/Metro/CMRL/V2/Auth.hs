{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (threadDelay)
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

refreshLockKey :: Text -> Text
refreshLockKey merchantId = "CMRLV2Auth:RefreshLock:" <> merchantId

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
getAuthToken config = do
  logDebug $ "[CMRLV2:Auth] Checking for cached auth token for merchantId: " <> config.merchantId
  authToken :: (Maybe Text) <- Hedis.get (authTokenKey config.merchantId)
  case authToken of
    Nothing -> do
      logDebug "[CMRLV2:Auth] No cached token found, requesting new token"
      resetAuthToken config
    Just token -> do
      logDebug "[CMRLV2:Auth] Using cached auth token"
      return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLV2Config -> m Text
resetAuthToken config = do
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
                  callAPI config.networkHostUrl (ET.client authAPI authReq) "authCMRLV2" authAPI
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
      logInfo $ "[CMRLV2:Auth] Redis lock already held by another pod for, waiting 2 seconds"
      threadDelay 2000000
      getAuthToken config

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
callCMRLV2API config eulerClientFunc description proxy =
  callCMRLV2APIWithRetry config eulerClientFunc description proxy 0

-- | Maximum number of retries for transient failures
cmrlMaxRetries :: Int
cmrlMaxRetries = 3

-- | Calculate exponential backoff delay in microseconds: 500ms, 1s, 2s
backoffDelayUs :: Int -> Int
backoffDelayUs attempt = 500000 * (2 ^ attempt) -- 500ms * 2^attempt

callCMRLV2APIWithRetry ::
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
  Int ->
  m res
callCMRLV2APIWithRetry config eulerClientFunc description proxy attempt = do
  logInfo $ "[CMRLV2:API] Calling API: " <> description <> " at " <> showBaseUrl config.networkHostUrl <> " (attempt " <> show (attempt + 1) <> ")"
  token <- getAuthToken config
  eitherResp <- withTryCatch "CMRLV2:auth" $ callApiUnwrappingApiError (identity @CMRLV2Error) Nothing Nothing Nothing config.networkHostUrl (eulerClientFunc token) description proxy
  case eitherResp of
    Left exec -> do
      let mbError = fromException @CMRLV2Error exec
          errorCode = mbError <&> toErrorCode
      logError $ "[CMRLV2:API] API call failed: " <> description <> ", errorCode: " <> show errorCode <> ", attempt: " <> show (attempt + 1)
      case errorCode of
        Just "UNAUTHORIZED" -> do
          if attempt < cmrlMaxRetries
            then do
              logInfo "[CMRLV2:API] Token expired, refreshing and retrying..."
              void $ resetAuthToken config
              callCMRLV2APIWithRetry config eulerClientFunc description proxy (attempt + 1)
            else do
              logError $ "[CMRLV2:API] All " <> show cmrlMaxRetries <> " retries exhausted for UNAUTHORIZED on: " <> description
              case mbError of
                Just err -> throwError err
                Nothing -> throwError $ InternalError "CMRL V2 API Failed: UNAUTHORIZED after max retries"
        _ -> do
          if attempt < cmrlMaxRetries
            then do
              let delayUs = backoffDelayUs attempt
              logWarning $ "[CMRLV2:API] Transient failure, retrying in " <> show (delayUs `div` 1000) <> "ms (attempt " <> show (attempt + 1) <> "/" <> show cmrlMaxRetries <> ")"
              threadDelay delayUs
              callCMRLV2APIWithRetry config eulerClientFunc description proxy (attempt + 1)
            else do
              logError $ "[CMRLV2:API] All " <> show cmrlMaxRetries <> " retries exhausted for: " <> description
              case mbError of
                Just err -> throwError err
                Nothing -> throwError $ InternalError "CMRL V2 API Failed"
    Right resp -> do
      logInfo $ "[CMRLV2:API] API call successful: " <> description
      return resp
