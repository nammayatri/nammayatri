module ExternalBPP.ExternalAPI.Subway.CRIS.Auth where

import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude hiding (threadDelay)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISErrorUnhandled (..))
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error.BaseError.HTTPError.FromResponse
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant
import Servant hiding (throwError)
import Tools.Error
import Tools.HTTPManager (crisHttpManagerKey)
import Tools.Metrics (CoreMetrics)

type AuthAPI =
  "token"
    :> BasicAuth "username-password" BasicAuthData
    :> Header "Content-Type" Text
    :> ReqBody '[FormUrlEncoded] [(Text, Text)]
    :> Post '[JSON] CRISTokenRes

data CRISTokenRes = CRISTokenRes
  { access_token :: Text,
    token_type :: Text,
    expires_in :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)

authAPI :: Proxy AuthAPI
authAPI = Proxy

mkBasicAuthData :: Text -> Text -> BasicAuthData
mkBasicAuthData userName password =
  BasicAuthData
    { basicAuthUsername = DT.encodeUtf8 userName,
      basicAuthPassword = DT.encodeUtf8 password
    }

getCRISTokenKey :: Text
getCRISTokenKey = "cris-token"

resetAuthToken ::
  (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m) =>
  CRISConfig ->
  m Text
resetAuthToken config = do
  lockAcquired <- Hedis.withCrossAppRedis $ Hedis.tryLockRedis getCRISTokenRefreshLockKey 30
  if lockAcquired
    then do
      logInfo "Acquired Redis lock for token refresh"

      let unlockLock = do
            logInfo "Released Redis lock after token refresh"
            Hedis.withCrossAppRedis $ Hedis.unlockRedis getCRISTokenRefreshLockKey

      tokenRes <-
        ( do
            consumerKey <- decrypt config.consumerKey
            consumerSecret <- decrypt config.consumerSecret
            let basicAuthData = mkBasicAuthData consumerKey consumerSecret
            callAPI config.baseUrl (ET.client authAPI basicAuthData (Just "application/x-www-form-urlencoded") [("grant_type", "client_credentials")]) "authCRIS" authAPI
              >>= fromEitherM (ExternalAPICallError (Just "CRIS_AUTH_API") config.baseUrl)
          )
          `finally` unlockLock

      Hedis.withCrossAppRedis $ Hedis.setExp getCRISTokenKey (tokenRes.access_token) (tokenRes.expires_in * 90 `div` 100)
      return $ tokenRes.access_token
    else do
      logInfo "Redis lock already held by another pod, waiting 3 seconds for token refresh"

      threadDelay 3000000

      mbToken <- Hedis.withCrossAppRedis $ Hedis.get getCRISTokenKey
      case mbToken of
        Nothing ->
          throwError $ CRISErrorUnhandled "Token refresh failed - no token available after waiting"
        Just token -> return token

getAuthToken ::
  (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r) =>
  CRISConfig ->
  m Text
getAuthToken config = do
  mbToken <- Hedis.withCrossAppRedis $ Hedis.get getCRISTokenKey
  case mbToken of
    Nothing -> resetAuthToken config
    Just token -> return token

callCRISAPI ::
  ( HasCallStack,
    CoreMetrics m,
    SanitizedUrl api,
    MonadFlow m,
    ToJSON res,
    CacheFlow m r,
    EncFlow m r,
    FromResponse CRISErrorUnhandled,
    HasRequestId r,
    MonadReader r m
  ) =>
  CRISConfig ->
  Proxy api ->
  (Text -> ET.EulerClient res) ->
  Text ->
  m res
callCRISAPI config proxy clientFn description = do
  token <- getAuthToken config
  eitherResp <-
    withTryCatch "CRIS:auth" $
      callApiUnwrappingApiError
        (identity @CRISErrorUnhandled)
        (Just $ ET.ManagerSelector $ T.pack crisHttpManagerKey)
        Nothing
        Nothing
        config.baseUrl
        (clientFn token)
        description
        proxy
  case eitherResp of
    Left err ->
      if is401Error err
        then do
          logInfo "Received 401 error, attempting token refresh with Redis lock"
          freshToken <- resetAuthToken config
          eitherRetryResp <-
            withTryCatch "callApiUnwrappingApiError:callCRISAPI:retry" $
              callApiUnwrappingApiError
                (identity @CRISErrorUnhandled)
                (Just $ ET.ManagerSelector $ T.pack crisHttpManagerKey)
                Nothing
                Nothing
                config.baseUrl
                (clientFn freshToken)
                description
                proxy
          case eitherRetryResp of
            Left retryErr ->
              if is401Error retryErr
                then throwError $ CRISErrorUnhandled $ "Authentication failed even after token refresh: " <> T.pack (show retryErr)
                else throwError $ CRISErrorUnhandled $ "Error while calling CRIS API after retry: " <> T.pack (show retryErr)
            Right res -> return res
        else throwError $ CRISErrorUnhandled $ "Error while calling CRIS API : " <> T.pack (show err)
    Right res -> return res
  where
    -- Helper function to check if error is 401
    is401Error :: SomeException -> Bool
    is401Error err =
      let errStr = show err
       in "401" `T.isInfixOf` T.pack errStr || "Unauthorized" `T.isInfixOf` T.pack errStr

getCRISTokenRefreshLockKey :: Text
getCRISTokenRefreshLockKey = "cris-token-refresh-lock"
