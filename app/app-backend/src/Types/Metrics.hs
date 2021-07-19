module Types.Metrics
  ( HasBAPMetrics,
    BAPMetricsContainer (..),
    module CoreMetrics,
    registerBAPMetricsContainer,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P
import Utils.Common

type HasBAPMetrics m r = (HasFlowEnv m r '["bapMetrics" ::: BAPMetricsContainer])

data BAPMetricsContainer = BAPMetricsContainer
  { searchRequestCounter :: SearchRequestCounterMetric,
    searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric
  }

type SearchRequestCounterMetric = P.Vector P.Label1 P.Counter

type SearchDurationMetric = (P.Histogram, P.Counter)

registerBAPMetricsContainer :: Seconds -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  searchRequestCounter <- registerSearchRequestCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  return $ BAPMetricsContainer {..}

registerSearchRequestCounterMetric :: IO SearchRequestCounterMetric
registerSearchRequestCounterMetric = P.register $ P.vector "status" $ P.counter $ P.Info "search_request_count" ""

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (getSeconds searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)
