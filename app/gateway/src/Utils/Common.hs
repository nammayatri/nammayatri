module Utils.Common where

import Beckn.Utils.Common (encodeToText')
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response

callAPI baseUrl req serviceName = do
  endTracking <- L.runUntracedIO $ Metrics.startTracking (encodeToText' baseUrl) serviceName
  res <- L.callAPI baseUrl req
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- L.runUntracedIO $ endTracking status
  return res
