{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Types.Error.APIError (IsAPIError (toErrorCode, toHttpCode), IsAPIException)
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Monitoring.Prometheus.Servant
import Data.Ratio ((%))
import Data.Text as DT
import qualified EulerHS.Language as L
import EulerHS.Prelude as E
import Network.Wai (Application, Request (..))
import Network.Wai.Handler.Warp as W
import Network.Wai.Internal (Response, ResponseReceived)
import Network.Wai.Middleware.Prometheus
import Prometheus as P
import Prometheus.Metric.GHC (ghcMetrics)
import Prometheus.Metric.Proc
import System.Clock (Clock (..), TimeSpec, diffTimeSpec, getTime, toNanoSecs)

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

startRequestLatencyTracking :: (L.MonadFlow m) => RequestLatencyMetric -> Text -> Text -> m (Text -> m ())
startRequestLatencyTracking requestLatencyMetric host serviceName = do
  start <- L.runIO $ getTime Monotonic
  return $ logRequestLatency requestLatencyMetric host serviceName start

logRequestLatency :: (L.MonadFlow m) => RequestLatencyMetric -> Text -> Text -> TimeSpec -> Text -> m ()
logRequestLatency requestLatencyMetric host serviceName start status = do
  end <- L.runIO $ getTime Monotonic
  let latency = fromRational $ toNanoSecs (end `diffTimeSpec` start) % 1000000000
  L.runIO $
    P.withLabel
      requestLatencyMetric
      (host, serviceName, status)
      (`P.observe` latency)

incrementErrorCounter :: (L.MonadFlow m, IsAPIException e) => ErrorCounterMetric -> e -> m ()
incrementErrorCounter errorCounterMetric err = 
  L.runIO $
    P.withLabel
      errorCounterMetric
      (show $ toHttpCode err, toErrorCode err)
      P.incCounter
