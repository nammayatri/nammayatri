{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (elem, threadDelay)
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

-- | Circuit breaker Redis key for CMRL V2 API failure tracking
cmrlV2CircuitBreakerKey :: Text -> Text
cmrlV2CircuitBreakerKey merchantId = "CMRLV2:CircuitBreaker:" <> merchantId

-- | Circuit breaker: window in seconds, failure threshold
cmrlV2CBWindowSec :: Int
cmrlV2CBWindowSec = 60

cmrlV2CBThreshold :: Int
cmrlV2CBThreshold = 10

-- | Call CMRL V2 API with retry, exponential backoff, and circuit breaker.
-- Circuit breaker: fails fast if >=10 failures in 60s window.
-- Retries up to 3 times for 500/503/504 errors with delays of 1s, 2s, 4s.
-- Auth token refresh is attempted once on 401.
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
  -- Circuit breaker: fail fast if CMRL V2 API has been consistently failing
  let cbKey = cmrlV2CircuitBreakerKey config.merchantId
  failureCount <- fromMaybe 0 <$> Hedis.get @Int cbKey
  when (failureCount >= cmrlV2CBThreshold) $ do
    logError $ "[CMRLV2:CircuitBreaker] Circuit OPEN for merchantId: " <> config.merchantId
      <> " (" <> show failureCount <> " failures in last " <> show cmrlV2CBWindowSec <> "s)"
      <> " — rejecting " <> description
    throwError $ InternalError $ "CMRL V2 API circuit breaker open: too many recent failures for " <> description
  callCMRLV2APIWithRetry config eulerClientFunc description proxy 0 False

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
  Int ->  -- retry attempt (0-based)
  Bool -> -- whether auth has already been refreshed
  m res
callCMRLV2APIWithRetry config eulerClientFunc description proxy attempt authRefreshed = do
  let cmrlMaxRetries = 3 :: Int
  logInfo $ "[CMRLV2:API] Calling API: " <> description <> " at " <> showBaseUrl config.networkHostUrl <> " (attempt " <> show (attempt + 1) <> ")"
  token <- getAuthToken config
  eitherResp <- withTryCatch "CMRLV2:auth" $ callApiUnwrappingApiError (identity @CMRLV2Error) Nothing Nothing Nothing config.networkHostUrl (eulerClientFunc token) description proxy
  case eitherResp of
    Left exec -> do
      let mbError = fromException @CMRLV2Error exec
          errorCode = mbError <&> toErrorCode
      logError $ "[CMRLV2:API] API call failed: " <> description <> ", errorCode: " <> show errorCode <> ", attempt: " <> show (attempt + 1)
      case errorCode of
        Just "UNAUTHORIZED"
          | not authRefreshed -> do
              logInfo "[CMRLV2:API] Token expired, refreshing and retrying (once)..."
              void $ resetAuthToken config
              callCMRLV2APIWithRetry config eulerClientFunc description proxy attempt True
          | otherwise -> do
              logError "[CMRLV2:API] Auth retry already attempted, failing"
              recordCMRLV2Failure config.merchantId
              throwError $ InternalError "CMRL V2 authentication failed after token refresh"
        Just code
          | code `elem` (["INTERNAL_ERROR", "SERVICE_UNAVAILABLE", "GATEWAY_TIMEOUT"] :: [Text]) && attempt < cmrlMaxRetries -> do
              let delayMs = 1000000 * (2 ^ attempt) -- 1s, 2s, 4s exponential backoff
              logWarning $ "[CMRLV2:API] Transient error (" <> code <> "), retrying in " <> show (delayMs `div` 1000000) <> "s..."
              threadDelay delayMs
              callCMRLV2APIWithRetry config eulerClientFunc description proxy (attempt + 1) authRefreshed
        _ -> do
          recordCMRLV2Failure config.merchantId
          case mbError of
            Just err -> throwError err
            Nothing -> throwError $ InternalError "CMRL V2 API Failed"
    Right resp -> do
      logInfo $ "[CMRLV2:API] API call successful: " <> description
      -- Clear circuit breaker on success
      let cbKey = cmrlV2CircuitBreakerKey config.merchantId
      void $ Hedis.del cbKey
      return resp

-- | Record a CMRL V2 API failure for circuit breaker tracking
recordCMRLV2Failure ::
  (MonadFlow m, CacheFlow m r) =>
  Text -> -- merchantId
  m ()
recordCMRLV2Failure merchantId = do
  let cbKey = cmrlV2CircuitBreakerKey merchantId
  count <- Hedis.incr cbKey
  when (count == 1) $ void $ Hedis.expire cbKey cmrlV2CBWindowSec
  when (count >= fromIntegral cmrlV2CBThreshold) $
    logError $ "[CMRLV2:CircuitBreaker] Threshold reached for merchantId: " <> merchantId <> " (" <> show count <> " failures)"
