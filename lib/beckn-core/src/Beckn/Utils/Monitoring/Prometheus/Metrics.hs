{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Types.Error.BaseError.HTTPError (IsHTTPError (toErrorCode, toHttpCode), IsHTTPException)
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer, HasCoreMetrics)
import Beckn.Utils.Monitoring.Prometheus.Servant
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
import Servant.Client (ClientError (..), ResponseF (..))

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

incrementErrorCounterFlow ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m,
    IsHTTPException e
  ) =>
  e ->
  m ()
incrementErrorCounterFlow err = do
  cmContainer <- asks (.coreMetrics)
  incrementErrorCounter cmContainer err

addRequestLatencyFlow ::
  ( HasCoreMetrics r,
    L.MonadFlow m,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Double ->
  Either ClientError a ->
  m ()
addRequestLatencyFlow host serviceName dur status = do
  cmContainer :: CoreMetricsContainer <- asks (.coreMetrics)
  let requestLatencyMetric = cmContainer.requestLatency
  L.runIO $
    P.withLabel
      requestLatencyMetric
      (host, serviceName, status')
      (`P.observe` dur)
  where
    status' =
      case status of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> show code
        Left (DecodeFailure _ (Response code _ _ _)) -> show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> show code
        Left (ConnectionError _) -> "Connection error"

incrementErrorCounter :: (L.MonadFlow m, IsHTTPException e) => CoreMetricsContainer -> e -> m ()
incrementErrorCounter cmContainers err = do
  let errorCounterMetric = cmContainers.errorCounter
  L.runIO $
    P.withLabel
      errorCounterMetric
      (show $ toHttpCode err, toErrorCode err)
      P.incCounter
