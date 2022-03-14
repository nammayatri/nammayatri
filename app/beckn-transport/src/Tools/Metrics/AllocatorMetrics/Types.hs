module Tools.Metrics.AllocatorMetrics.Types
  ( HasAllocatorMetrics,
    AllocatorMetricsContainer (..),
    module CoreMetrics,
    registerAllocatorMetricsContainer,
  )
where

import Beckn.Tools.Metrics.CoreMetrics as CoreMetrics
import EulerHS.Prelude
import Prometheus as P
import Utils.Common

type HasAllocatorMetrics m r = (HasFlowEnv m r '["btmMetrics" ::: AllocatorMetricsContainer])

type TaskCounterMetric = P.Counter

type TaskDurationMetric = P.Histogram

type FailedTaskCounterMetric = P.Counter

data AllocatorMetricsContainer = AllocatorMetricsContainer
  { taskCounter :: TaskCounterMetric,
    taskDuration :: TaskDurationMetric,
    failedTaskCounter :: FailedTaskCounterMetric
  }

registerAllocatorMetricsContainer :: IO AllocatorMetricsContainer
registerAllocatorMetricsContainer = do
  taskCounter <- registerTaskCounter
  taskDuration <- registerTaskDurationMetric
  failedTaskCounter <- registerFailedTaskCounter
  return $ AllocatorMetricsContainer {..}

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.counter $ P.Info "BTM_task_count" ""

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.counter $ P.Info "BTM_failed_task_count" ""

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20
