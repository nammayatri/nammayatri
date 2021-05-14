module Types.Metrics
  ( module Types.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P

type CaseCounterMetric = P.Vector P.Label2 P.Counter

type SearchDurationMetric = (P.Histogram, P.Counter)

registerCaseCounter :: IO CaseCounterMetric
registerCaseCounter = P.register $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

registerSearchDurationMetric :: Int -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)
