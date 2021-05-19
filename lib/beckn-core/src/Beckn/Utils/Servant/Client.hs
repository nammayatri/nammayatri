module Beckn.Utils.Servant.Client where

import Beckn.Types.Common
import Beckn.Types.Monitoring.Prometheus.Metrics (RequestLatencyMetric)
import Beckn.Utils.Logging (logInfo)
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import GHC.Records
import Servant.Client.Core

callAPI ::
  (HasCallStack, Log (FlowR r), HasField "metricsRequestLatency" r RequestLatencyMetric, ET.JSONEx a, ToJSON a) =>
  BaseUrl ->
  ET.EulerClient a ->
  Text ->
  FlowR r (Either ClientError a)
callAPI = callAPI' Nothing

callAPI' ::
  (HasCallStack, Log (FlowR r), HasField "metricsRequestLatency" r RequestLatencyMetric, ET.JSONEx a, ToJSON a) =>
  Maybe ET.ManagerSelector ->
  BaseUrl ->
  ET.EulerClient a ->
  Text ->
  FlowR r (Either ClientError a)
callAPI' mbManagerSelector baseUrl eulerClient desc = do
  withLogTag "callAPI" $ do
    endTracking <- Metrics.startRequestLatencyTracking (T.pack $ showBaseUrl baseUrl) desc
    res <- L.callAPI' mbManagerSelector baseUrl eulerClient
    case res of
      Right r -> logInfo $ "Ok response: " <> decodeUtf8 (A.encode r)
      Left err -> logInfo $ "Error occured during client call: " <> show err
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
