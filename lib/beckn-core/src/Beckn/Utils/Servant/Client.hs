module Beckn.Utils.Servant.Client where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Error.API
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import qualified Servant.Client as S
import Servant.Client.Core
import qualified Data.Aeson as A
import Beckn.Utils.Logging (logInfo)

callClient ::
  (ET.JSONEx a, L.MonadFlow m, HasCoreMetrics m, Log m) =>
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient = callClient' Nothing

-- TODO: the @desc@ argument should become part of monadic context
callClient' ::
  (ET.JSONEx a, L.MonadFlow m, HasCoreMetrics m, Log m) =>
  Maybe String ->
  Text ->
  context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient' mbManager desc _ baseUrl cli = do
  endTracking <- startRequestLatencyTracking (T.pack $ showBaseUrl baseUrl) desc
  res <- L.callAPI' mbManager baseUrl cli
  _ <- endTracking $ getResponseCode res
  res & fromEitherM (ExternalAPICallError baseUrl)
  where
    getResponseCode res =
      case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"

callAPI ::
  (HasCallStack, L.MonadFlow m, Log m, ET.JSONEx a, ToJSON a) =>
  BaseUrl ->
  ET.EulerClient a ->
  m (Either ClientError a)
callAPI = callAPI' Nothing

callAPI' ::
  (HasCallStack, L.MonadFlow m, Log m, ET.JSONEx a, ToJSON a) =>
  Maybe ET.ManagerSelector ->
  BaseUrl ->
  ET.EulerClient a ->
  m (Either ClientError a)
callAPI' mbManagerSelector baseUrl eulerClient = do
  withLogTag "callAPI" $ do
    res <- L.callAPI' mbManagerSelector baseUrl eulerClient
    case res of
      Right r -> logInfo $ "Ok response: " <> decodeUtf8 (A.encode r)
      Left err -> logInfo $ "Error occured during client call: " <> show err
    return res