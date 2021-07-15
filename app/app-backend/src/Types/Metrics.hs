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
  { caseCounter :: CaseCounterMetric,
    searchDurationTimeout :: Int,
    searchDuration :: SearchDurationMetric
  }

type CaseCounterMetric = P.Vector P.Label2 P.Counter

type SearchDurationMetric = (P.Histogram, P.Counter)

registerBAPMetricsContainer :: Int -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  caseCounter <- registerCaseCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  return $ BAPMetricsContainer {..}

registerCaseCounterMetric :: IO CaseCounterMetric
registerCaseCounterMetric = P.register $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

registerSearchDurationMetric :: Int -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)
