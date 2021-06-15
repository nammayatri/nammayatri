module Beckn.Utils.Servant.Client where

import Beckn.Types.Common
import Beckn.Types.Error.APIError
import Beckn.Types.Error.CallAPIError
import Beckn.Types.Error.FromResponse
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import Beckn.Utils.Logging 
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import qualified Servant.Client as S
import Servant.Client.Core

type CallAPI' env res res' =
  ( HasCallStack,
    Metrics.HasCoreMetrics env,
    ET.JSONEx res,
    ToJSON res
  ) =>
  BaseUrl ->
  ET.EulerClient res ->
  Text ->
  FlowR env res'

type CallAPI env res = CallAPI' env res res

callAPI :: CallAPI' env res (Either ClientError res)
callAPI = callAPI' Nothing

callAPI' ::
  Maybe ET.ManagerSelector ->
  CallAPI' env res (Either ClientError res)
callAPI' mbManagerSelector baseUrl eulerClient desc =
  withLogTag "callAPI" $ do
    endTracking <- Metrics.startRequestLatencyTracking (T.pack $ showBaseUrl baseUrl) desc
    res <- L.callAPI' mbManagerSelector baseUrl eulerClient
    case res of
      Right r -> logDebug $ "Ok response: " <> decodeUtf8 (A.encode r)
      Left err -> logDebug $ "Error occured during client call: " <> show err
    _ <- endTracking $ getResponseCode res
    return res
  where
    getResponseCode res =
      case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"

parseBaseUrl :: MonadThrow m => Text -> m S.BaseUrl
parseBaseUrl = S.parseBaseUrl . T.unpack

callApiExtractingApiError ::
  FromResponse err =>
  Maybe ET.ManagerSelector ->
  CallAPI' env a (Either (CallAPIError err) a)
callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc =
  callAPI' mbManagerSelector baseUrl eulerClient desc
    <&> extractApiError

callApiUnwrappingApiError ::
  ( FromResponse err,
    IsAPIException exc
  ) =>
  (err -> exc) ->
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callApiUnwrappingApiError toAPIException mbManagerSelector errorCodeMb baseUrl eulerClient desc =
  callApiExtractingApiError mbManagerSelector baseUrl eulerClient desc
    >>= unwrapEitherCallAPIError errorCodeMb baseUrl toAPIException
