{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Monitoring.Prometheus.Metrics where

import Beckn.Utils.Monitoring.Prometheus.Servant
import Data.Ratio ((%))
import Data.Text as DT
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

requestLatency :: P.Vector P.Label3 P.Histogram
requestLatency =
  P.unsafeRegister $
    P.vector ("host", "service", "status") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

startTracking :: Text -> Text -> IO (Text -> IO ())
startTracking host serviceName = do
  start <- getTime Monotonic
  return $ logRequestLatency host serviceName start

logRequestLatency :: Text -> Text -> TimeSpec -> Text -> IO ()
logRequestLatency host serviceName start status = do
  end <- getTime Monotonic
  let latency = fromRational $ toNanoSecs (end `diffTimeSpec` start) % 1000000000
  P.withLabel
    requestLatency
    (host, serviceName, status)
    (`P.observe` latency)
