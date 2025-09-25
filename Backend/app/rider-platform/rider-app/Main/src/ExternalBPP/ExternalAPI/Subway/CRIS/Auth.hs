module ExternalBPP.ExternalAPI.Subway.CRIS.Auth where

import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Domain.Types.Extra.IntegratedBPPConfig
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError)
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
  (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  CRISConfig ->
  m Text
resetAuthToken config = do
  consumerKey <- decrypt config.consumerKey
  consumerSecret <- decrypt config.consumerSecret
  let basicAuthData = mkBasicAuthData consumerKey consumerSecret
  tokenRes <-
    callAPI config.baseUrl (ET.client authAPI basicAuthData (Just "application/x-www-form-urlencoded") [("grant_type", "client_credentials")]) "authCRIS" authAPI
      >>= fromEitherM (ExternalAPICallError (Just "CRIS_AUTH_API") config.baseUrl)
  Hedis.setExp getCRISTokenKey (tokenRes.access_token) (tokenRes.expires_in * 90 `div` 100)
  return $ tokenRes.access_token

getAuthToken ::
  (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  CRISConfig ->
  m Text
getAuthToken config = do
  mbToken <- Hedis.get getCRISTokenKey
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
    FromResponse CRISError
  ) =>
  CRISConfig ->
  Proxy api ->
  (Text -> ET.EulerClient res) ->
  Text ->
  m res
callCRISAPI config proxy clientFn description = do
  token <- getAuthToken config
  eitherResp <-
    try @_ @SomeException $
      callApiUnwrappingApiError
        (identity @CRISError) -- Changed Error to CRISError
        (Just $ ET.ManagerSelector $ T.pack crisHttpManagerKey)
        Nothing
        Nothing
        config.baseUrl
        (clientFn token)
        description
        proxy
  case eitherResp of
    Left err -> throwError $ InternalError $ "Error while calling CRIS API" <> (show err)
    Right res -> return res
