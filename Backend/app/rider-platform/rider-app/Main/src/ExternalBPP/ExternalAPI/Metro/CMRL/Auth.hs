{-# LANGUAGE OverloadedLists #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.Auth where

import Data.Aeson
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (threadDelay)
import EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Metro.CMRL.Error
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

getAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLConfig -> m Text
getAuthToken config = do
  authToken :: (Maybe Text) <- Hedis.runInMultiCloudRedisMaybeResult $ Hedis.get authTokenKey
  case authToken of
    Nothing -> resetAuthToken config
    Just token -> return token

resetAuthToken :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => CMRLConfig -> m Text
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
                  runThroughMasterCloud config.networkHostUrl (ET.client authAPI $ AuthReq config.username password cmrlAppType) "authCMRL"
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
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m,
    HasMasterCloudForwarder r
  ) =>
  CMRLConfig ->
  (Text -> ET.EulerClient res) ->
  Text ->
  Proxy api ->
  m res
callCMRLAPI config eulerClientFunc description _proxy = do
  token <- getAuthToken config
  result <- runThroughMasterCloud config.networkHostUrl (eulerClientFunc token) description
  case result of
    Right resp -> return resp
    Left clientErr -> case clientErr of
      FailureResponse _req resp -> do
        let code = HTTP.statusCode (SC.responseStatusCode resp)
        if code == 401
          then do
            logError "[CMRL:API] Token expired, refreshing and retrying..."
            void $ resetAuthToken config
            callCMRLAPI config eulerClientFunc description _proxy
          else case fromResponse @CMRLError resp of
            Just cmrlErr -> throwError cmrlErr
            Nothing -> throwError $ InternalError "CMRL API Failed"
      _ -> throwError $ InternalError "CMRL API Failed"
