module Beckn.Types.Monitoring.Prometheus.Metrics where

import Beckn.Types.Error.APIError
import EulerHS.Prelude as E
import GHC.Records.Extra
import Prometheus as P

type RequestLatencyMetric = P.Vector P.Label3 P.Histogram

type ErrorCounterMetric = P.Vector P.Label2 P.Counter

type HasCoreMetrics r =
  ( HasField "metricsRequestLatency" r RequestLatencyMetric,
    HasField "metricsErrorCounter" r ErrorCounterMetric
  )

class CoreMetrics m where
  startRequestLatencyTracking :: Text -> Text -> m (Text -> m ())
  incrementErrorCounter :: IsAPIException e => e -> m ()

registerRequestLatencyMetric :: IO RequestLatencyMetric
registerRequestLatencyMetric =
  P.register $
    P.vector ("host", "service", "status") $
      P.histogram info P.defaultBuckets
  where
    info = P.Info "external_request_duration" ""

registerErrorCounterMetric :: IO ErrorCounterMetric
registerErrorCounterMetric =
  P.register $
    P.vector ("HttpCode", "ErrorCode") $
      P.counter info
  where
    info = P.Info "error_counter" ""
