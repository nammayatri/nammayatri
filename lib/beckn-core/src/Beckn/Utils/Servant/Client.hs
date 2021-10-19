module Beckn.Utils.Servant.Client where

import Beckn.Types.Common
import Beckn.Types.Error (ExternalAPICallError (..))
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.CallAPIError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Beckn.Utils.Servant.BaseUrl
import Beckn.Utils.Time
import qualified Data.Aeson as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import GHC.Records.Extra (HasField)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Servant.Client as S
import Servant.Client.Core

data HttpClientOptions = HttpClientOptions
  { timeoutMs :: Int,
    maxRetries :: Int
  }
  deriving (Generic, FromDhall)

type HasHttpClientOptions r = HasField "httpClientOptions" r HttpClientOptions

type CallAPI' m res res' =
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m,
    ET.JSONEx res,
    ToJSON res
  ) =>
  BaseUrl ->
  ET.EulerClient res ->
  Text ->
  m res'

type CallAPI m res = CallAPI' m res res

callAPI ::
  CallAPI' m res (Either ClientError res)
callAPI = callAPI' Nothing

callAPI' ::
  Maybe ET.ManagerSelector ->
  CallAPI' m res (Either ClientError res)
callAPI' mbManagerSelector baseUrl eulerClient desc =
  withLogTag "callAPI" $ do
    let managerSelector = fromMaybe defaultHttpManager mbManagerSelector
    res <-
      measuringDuration (Metrics.addRequestLatency (showBaseUrlText baseUrl) desc) $
        L.callAPI' (Just managerSelector) baseUrl eulerClient
    case res of
      Right r -> logDebug $ "Ok response: " <> decodeUtf8 (A.encode r)
      Left err -> logDebug $ "Error occured during client call: " <> show err
    return res

parseBaseUrl :: MonadThrow m => Text -> m S.BaseUrl
parseBaseUrl = S.parseBaseUrl . T.unpack

callApiExtractingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err
  ) =>
  Maybe ET.ManagerSelector ->
  CallAPI' m a (Either (CallAPIError err) a)
callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc =
  callAPI' mbManagerSelector baseUrl eulerClient desc
    <&> extractApiError

callApiUnwrappingApiError ::
  ( Metrics.CoreMetrics m,
    FromResponse err,
    IsHTTPException exc
  ) =>
  (err -> exc) ->
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI m a
callApiUnwrappingApiError toAPIException mbManagerSelector errorCodeMb baseUrl eulerClient desc =
  callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc
    >>= unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException

defaultHttpManager :: String
defaultHttpManager = "default"

setResponseTimeout :: Int -> Http.ManagerSettings -> Http.ManagerSettings
setResponseTimeout timeout settings =
  settings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

createManagers ::
  ( MonadReader r m,
    HasHttpClientOptions r,
    MonadFlow m
  ) =>
  Map String Http.ManagerSettings ->
  m (Map String Http.Manager)
createManagers managerSettings = do
  timeout <- asks (.httpClientOptions.timeoutMs)
  liftIO
    . mapM Http.newManager
    . fmap (setResponseTimeout timeout)
    . Map.insert defaultHttpManager Http.tlsManagerSettings
    $ managerSettings

catchConnectionErrors :: (MonadCatch m, Log m) => m a -> (ExternalAPICallError -> m a) -> m a
catchConnectionErrors action errorHandler =
  action `catch` \err -> do
    case err.clientError of
      ConnectionError _ -> errorHandler err
      _ -> throwError err

retryAction ::
  ( MonadCatch m,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  ExternalAPICallError ->
  Int ->
  Int ->
  m a ->
  m a
retryAction currentErr currentRetryCount maxRetries action = do
  logWarning $ "Error calling " <> showBaseUrlText currentErr.baseUrl <> ": " <> show currentErr.clientError
  logWarning $ "Retrying attempt " <> show currentRetryCount <> " calling " <> showBaseUrlText currentErr.baseUrl
  Metrics.addUrlCallRetries currentErr.baseUrl currentRetryCount
  catchConnectionErrors action $ \err -> do
    if currentRetryCount < maxRetries
      then retryAction err (currentRetryCount + 1) maxRetries action
      else do
        logError $ "Maximum of retrying attempts is reached calling " <> showBaseUrlText err.baseUrl
        Metrics.addUrlCallRetryFailures currentErr.baseUrl
        throwError err

withRetry ::
  ( MonadCatch m,
    MonadReader r m,
    HasHttpClientOptions r,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  m a ->
  m a
withRetry action = do
  maxRetries <- asks (.httpClientOptions.maxRetries)
  catchConnectionErrors action $ \err -> do
    if maxRetries > 0
      then retryAction err 1 maxRetries action
      else do
        Metrics.addUrlCallRetryFailures err.baseUrl
        throwError err
