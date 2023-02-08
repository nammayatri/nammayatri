module Tools.Metrics.BAPMetrics.Types
  ( HasBAPMetrics,
    BAPMetricsContainer (..),
    registerBAPMetricsContainer,
  )
where

import Kernel.Prelude
import Kernel.Utils.Common
import Prometheus as P

type HasBAPMetrics m r = (HasFlowEnv m r '["bapMetrics" ::: BAPMetricsContainer])

data BAPMetricsContainer = BAPMetricsContainer
  { searchRequestCounter :: SearchRequestCounterMetric,
    searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric
  }

type SearchRequestCounterMetric = P.Vector P.Label1 P.Counter

type SearchDurationMetric = (P.Vector P.Label1 P.Histogram, P.Vector P.Label1 P.Counter)

registerBAPMetricsContainer :: Seconds -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  searchRequestCounter <- registerSearchRequestCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  return $ BAPMetricsContainer {..}

registerSearchRequestCounterMetric :: IO SearchRequestCounterMetric
registerSearchRequestCounterMetric = P.register $ P.vector "merchant_name" $ P.counter $ P.Info "search_request_count" ""

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (getSeconds searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.vector "merchant_name" . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register . P.vector "merchant_name" $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)
