module Types.Metrics
  ( BTMMetrics (..),
    BTMMetricsContainer (..),
    module CoreMetrics,
    registerBTMMetricsContainer,
  )
where

import Beckn.Types.Monitoring.Prometheus.Metrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P

class BTMMetrics m where
  incrementTaskCounter :: m ()
  incrementFailedTaskCounter :: m ()
  putTaskDuration :: Double -> m ()

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
