{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Types.Error.APIError (IsAPIError (toErrorCode, toHttpCode), IsAPIException)
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetricsContainer)
import Beckn.Utils.Monitoring.Prometheus.Servant
import Data.Ratio ((%))
import Data.Text as DT
import qualified EulerHS.Language as L
import EulerHS.Prelude as E
import GHC.Records.Extra
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Internal (Response, ResponseReceived)
import Network.Wai.Middleware.Prometheus
import Prometheus as P
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import System.Clock (Clock (..), diffTimeSpec, getTime, toNanoSecs)

serve :: Int -> IO ()
serve port = do
  _ <- register ghcMetrics
  _ <- register procMetrics
  putStrLn @String $ "Prometheus server started at port " <> show port
  _ <- forkIO $ W.run port metricsApp
  return ()

addServantInfo ::
  forall k (a :: k).
  SanitizedUrl a =>
  Proxy a ->
  Application ->
  Request ->
  ( Network.Wai.Internal.Response ->
    IO Network.Wai.Internal.ResponseReceived
  ) ->
  IO Network.Wai.Internal.ResponseReceived
addServantInfo proxy app request respond =
  let mpath = getSanitizedUrl proxy request
      fullpath = DT.intercalate "/" (pathInfo request)
   in instrumentHandlerValue (\_ -> "/" <> fromMaybe fullpath mpath) app request respond

startRequestLatencyTrackingFlow ::
  (HasField "coreMetrics" r CoreMetricsContainer, L.MonadFlow m, MonadReader r m) =>
  Text ->
  Text ->
  m (Text -> m ())
startRequestLatencyTrackingFlow host serviceName = do
  cmContainer <- asks (.coreMetrics)
  startRequestLatencyTracking cmContainer host serviceName

incrementErrorCounterFlow ::
  ( HasField "coreMetrics" r CoreMetricsContainer,
    L.MonadFlow m,
    MonadReader r m,
    IsAPIException e
  ) =>
  e ->
  m ()
incrementErrorCounterFlow err = do
  cmContainer <- asks (.coreMetrics)
  incrementErrorCounter cmContainer err

startRequestLatencyTracking :: (L.MonadFlow m) => CoreMetricsContainer -> Text -> Text -> m (Text -> m ())
startRequestLatencyTracking cmContainers host serviceName = do
  let requestLatencyMetric = cmContainers.requestLatency
  start <- L.runIO $ getTime Monotonic
  return $ logRequestLatency requestLatencyMetric start
  where
    logRequestLatency requestLatencyMetric start status = do
      end <- L.runIO $ getTime Monotonic
      let latency = fromRational $ toNanoSecs (end `diffTimeSpec` start) % 1000000000
      L.runIO $
        P.withLabel
          requestLatencyMetric
          (host, serviceName, status)
          (`P.observe` latency)

incrementErrorCounter :: (L.MonadFlow m, IsAPIException e) => CoreMetricsContainer -> e -> m ()
incrementErrorCounter cmContainers err = do
  let errorCounterMetric = cmContainers.errorCounter
  L.runIO $
    P.withLabel
      errorCounterMetric
      (show $ toHttpCode err, toErrorCode err)
      P.incCounter
