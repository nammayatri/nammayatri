module Types.Metrics
  ( BAPMetrics (..),
    BAPMetricsContainer (..),
    module CoreMetrics,
    registerBAPMetricsContainer,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified Beckn.Types.Storage.Case as Case
import EulerHS.Prelude
import Prometheus as P

class BAPMetrics m where
  startSearchMetrics :: Text -> m ()
  finishSearchMetrics :: Text -> m ()
  incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> m ()

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
