module Beckn.Types.Monitoring.Prometheus.Metrics where

import EulerHS.Prelude as E
import GHC.Records
import Prometheus as P

type RequestLatencyMetric = P.Vector P.Label3 P.Histogram

type HasCoreMetrics r = HasField "metricsRequestLatency" r RequestLatencyMetric

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""
