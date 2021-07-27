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
      measuringDuration (Metrics.addRequestLatency (T.pack $ showBaseUrl baseUrl) desc) $
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
  L.runIO
    . mapM Http.newManager
    . fmap (setResponseTimeout timeout)
    . Map.insert defaultHttpManager Http.tlsManagerSettings
    $ managerSettings

withRetry ::
  ( MonadCatch m,
    MonadReader r m,
    HasHttpClientOptions r,
    Metrics.CoreMetrics m,
    Log m
  ) =>
  m a ->
  m a
withRetry f = do
  asks (.httpClientOptions.maxRetries) >>= withRetry' Nothing 0
  where
    withRetry' mbMetricsInfo n maxRetries
      | n < maxRetries = do
        funcWithMetrics mbMetricsInfo maxRetries `catch` \(ExternalAPICallError _ baseUrl _) -> do
          when (n > 0) $
            logError $ "Retrying attempt " <> show n <> " calling " <> toText (showBaseUrl baseUrl)
          withRetry' (Just (baseUrl, succ n)) (succ n) maxRetries
      | otherwise =
        funcWithMetrics mbMetricsInfo maxRetries `catch` \err@(ExternalAPICallError _ baseUrl _) -> do
          logError $ "Maximum of retrying attempts is reached calling " <> toText (showBaseUrl baseUrl)
          throwError err
    funcWithMetrics mbMetricsInfo maxRetries = do
      rez <- f
      whenJust mbMetricsInfo $ \(baseUrl, n) ->
        Metrics.addUrlCallRetries baseUrl n maxRetries
      return rez
