{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (threadDelay)
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Error
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
  { username :: Text,
    password :: Text,
    appType :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data AuthRes = AuthRes
  { statusCode :: Int,
    message :: Text,
    result :: TokenResult
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype TokenResult = TokenResult
  { access_token :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type AuthAPI =
  "CmrlThirdParty" :> "authenticate"
    :> ReqBody '[JSON] AuthReq
    :> Post '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

authTokenKey :: Text
authTokenKey = "CMRLAuth:Token"

refreshLockKey :: Text
refreshLockKey = "CMRLAuth:RefreshLock"

cmrlAppType :: Text
cmrlAppType = "CMRL_CUM_IQR"

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> m Text
getAuthToken config = do
  Hedis.runInMasterCloudRedisCell $ do
    authToken :: (Maybe Text) <- Hedis.get authTokenKey
    case authToken of
      Nothing -> resetAuthToken config
      Just token -> return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) => CMRLConfig -> m Text
resetAuthToken config = do
  lockAcquired <- Hedis.tryLockRedis refreshLockKey 30
  if lockAcquired
    then do
      logInfo "[CMRL:Auth] Acquired Redis lock for token refresh"
      let unlockLock = do
            logInfo "[CMRL:Auth] Releasing Redis lock"
            Hedis.unlockRedis refreshLockKey

      auth <-
        ( do
            -- Double check cache
            mbToken <- Hedis.get authTokenKey
            case mbToken of
              Just token -> do
                logInfo "[CMRL:Auth] Token found in cache after acquiring lock"
                return token
              Nothing -> do
                logInfo $ "[CMRL:Auth] Requesting new auth token from: " <> showBaseUrl config.networkHostUrl
                password <- decrypt config.password
                authRes <-
                  callAPI config.networkHostUrl (ET.client authAPI $ AuthReq config.username password cmrlAppType) "authCMRL" authAPI
                    >>= fromEitherM (ExternalAPICallError (Just "CMRL_AUTH_API") config.networkHostUrl)
                logInfo "[CMRL:Auth] Successfully obtained auth token"
                let tokenExpiry = 2 * 3600
                Hedis.setExp authTokenKey authRes.result.access_token tokenExpiry
                return authRes.result.access_token
          )
          `finally` unlockLock
      return auth
    else do
      logInfo "[CMRL:Auth] Redis lock already held by another pod, waiting 2 seconds"
      threadDelay 2000000
      getAuthToken config

callCMRLAPI ::
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
  CMRLConfig ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  m res
callCMRLAPI config eulerClientFunc description proxy = do
  token <- getAuthToken config
  eitherResp <- withTryCatch "CMRL:auth" $ callApiUnwrappingApiError (identity @CMRLError) Nothing Nothing Nothing config.networkHostUrl (eulerClientFunc token) description proxy
  case eitherResp of
    Left exec -> do
      let mbError = fromException @CMRLError exec
          errorCode = mbError <&> toErrorCode
      case errorCode of
        Just "UNAUTHORIZED" -> do
          void $ resetAuthToken config
          callCMRLAPI config eulerClientFunc description proxy
        _ -> do
          case mbError of
            Just err -> throwError err
            Nothing -> throwError $ InternalError "CMRL API Failed"
    Right resp -> return resp
