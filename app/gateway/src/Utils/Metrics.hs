{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.Metrics () where

import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude

instance CoreMetrics.HasCoreMetrics Flow where
  startRequestLatencyTracking host serviceName = do
    appEnv <- ask
    CoreMetrics.startRequestLatencyTracking' (metricsRequestLatency appEnv) host serviceName
