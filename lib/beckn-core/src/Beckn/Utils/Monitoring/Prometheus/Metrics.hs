{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Error.BaseError.HTTPError (BaseException (..), HTTPException (..), IsBaseError (toMessage), IsHTTPError (toErrorCode, toHttpCode), IsHTTPException)
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer, HasCoreMetrics)
import Beckn.Types.Time (Milliseconds, getMilliseconds)
import Beckn.Utils.Monitoring.Prometheus.Servant
import Beckn.Utils.Servant.BaseUrl
import Data.Text as DT
import qualified EulerHS.Language as L
import EulerHS.Prelude as E
import GHC.Records.Extra
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Prometheus
import Prometheus as P
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import Servant.Client (BaseUrl, ClientError (..), ResponseF (..))

serve :: Int -> IO ()
serve port = do
  _ <- register ghcMetrics
  _ <- register procMetrics
  putStrLn @String $ "Prometheus server started at port " <> show port
  _ <- forkIO $ W.run port metricsApp
  return ()

addServantInfo ::
  SanitizedUrl a =>
  Proxy a ->
  Application ->
  Application
addServantInfo proxy app request respond =
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValue (\_ -> "/" <> fromMaybe fullpath mpath) app request respond

incrementErrorCounter ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  SomeException ->
  m ()
incrementErrorCounter errorContext err = do
  cmContainer <- asks (.coreMetrics)
  incrementErrorCounter' cmContainer errorContext err

addUrlCallRetries ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  Int ->
  m ()
addUrlCallRetries url retryCount = do
  cmContainer <- asks (.coreMetrics)
  addUrlCallRetries' cmContainer url retryCount

addUrlCallFailures ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  BaseUrl ->
  m ()
addUrlCallFailures url = do
  cmContainer <- asks (.coreMetrics)
  addUrlCallFailures' cmContainer url

addRequestLatency ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  m ()
addRequestLatency host serviceName dur status = do
  cmContainer <- asks (.coreMetrics)
  addRequestLatency' cmContainer host serviceName dur status

addRequestLatency' ::
  L.MonadFlow m =>
  CoreMetricsContainer ->
  Text ->
  Text ->
  Milliseconds ->
  Either ClientError a ->
  m ()
addRequestLatency' cmContainer host serviceName dur status = do
  let requestLatencyMetric = cmContainer.requestLatency
  L.runIO $
    P.withLabel
      requestLatencyMetric
      (host, serviceName, status')
      (`P.observe` ((/ 1000) . fromIntegral $ getMilliseconds dur))
  where
    status' =
      case status of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> show code
        Left (DecodeFailure _ (Response code _ _ _)) -> show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> show code
        Left (ConnectionError _) -> "Connection error"

incrementErrorCounter' :: L.MonadFlow m => CoreMetricsContainer -> Text -> SomeException -> m ()
incrementErrorCounter' cmContainers errorContext exc
  | Just (HTTPException err) <- fromException exc = incCounter' err
  | Just (BaseException err) <- fromException exc = incCounter' . InternalError . fromMaybe (show err) $ toMessage err
  | otherwise = incCounter' . InternalError $ show exc
  where
    errorCounterMetric = cmContainers.errorCounter

    incCounter' :: (L.MonadFlow m, IsHTTPException e) => e -> m ()
    incCounter' err =
      L.runIO $
        P.withLabel
          errorCounterMetric
          (show $ toHttpCode err, errorContext, toErrorCode err)
          P.incCounter

addUrlCallRetries' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> Int -> m ()
addUrlCallRetries' cmContainers url retryCount = do
  let urlCallRetriesMetric = cmContainers.urlCallRetries
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url, show retryCount)
      P.incCounter

addUrlCallFailures' :: L.MonadFlow m => CoreMetricsContainer -> BaseUrl -> m ()
addUrlCallFailures' cmContainers url = do
  let urlCallRetriesMetric = cmContainers.urlCallRetryFailures
  L.runIO $
    P.withLabel
      urlCallRetriesMetric
      (showBaseUrlText url)
      P.incCounter