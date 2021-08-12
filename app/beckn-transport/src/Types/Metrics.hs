module Types.Metrics
  ( HasBTMMetrics,
    BTMMetricsContainer (..),
    module CoreMetrics,
    registerBTMMetricsContainer,
    registerTransporterMetricsContainer,
    TransporterMetricsContainer (..),
    HasTransporterMetrics,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P
import Utils.Common

type HasBTMMetrics m r = (HasFlowEnv m r '["btmMetrics" ::: BTMMetricsContainer])

type TaskCounterMetric = P.Counter

type TaskDurationMetric = P.Histogram

type FailedTaskCounterMetric = P.Counter

data BTMMetricsContainer = BTMMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerBTMMetricsContainer :: IO BTMMetricsContainer
registerBTMMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ BTMMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20

type HasTransporterMetrics m r = HasFlowEnv m r '["transporterMetrics" ::: TransporterMetricsContainer]

data TransporterMetricsContainer = TransporterMetricsContainer
  { realFareDeviation :: P.Histogram,
    realDistanceDeviation :: P.Histogram
  }

registerTransporterMetricsContainer :: IO TransporterMetricsContainer
registerTransporterMetricsContainer =
  TransporterMetricsContainer
    <$> (P.register . P.histogram fareDeviation $ aroundZero 10 5)
    <*> (P.register . P.histogram distanceDeviation $ aroundZero 10 6)
  where
    aroundZero factor b =
      let l = P.exponentialBuckets 1 factor b
       in reverse (map negate l) ++ l
    fareDeviation =
      P.Info
        "BPP_fare_deviation"
        "Difference between initially offered and recalculated fare of a ride"
    distanceDeviation =
      P.Info
        "BPP_distance_deviation"
        "Difference between estimated distance and real distance of a ride"
