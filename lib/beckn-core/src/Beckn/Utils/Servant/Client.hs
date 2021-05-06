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
  endTracking <- Metrics.startTracking (T.pack $ showBaseUrl baseUrl) desc
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
