module Utils.Metrics
  ( TaskCounterMetric,
    TaskDurationMetric,
    FailedTaskCounterMetric,
    registerTaskCounter,
    incrementTaskCounter,
    registerFailedTaskCounter,
    incrementFailedTaskCounter,
    registerTaskDurationMetric,
    putTaskDuration,
    module CoreMetrics,
  )
where

import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

type TaskCounterMetric = P.Counter

type TaskDurationMetric = P.Histogram

type FailedTaskCounterMetric = P.Counter

registerTaskCounter :: IO TaskCounterMetric
registerTaskCounter = P.register . P.counter $ P.Info "BTM_task_count" ""

incrementTaskCounter :: (L.MonadFlow m) => TaskCounterMetric -> m ()
incrementTaskCounter taskCounter =
  L.runIO $ P.incCounter taskCounter

registerFailedTaskCounter :: IO FailedTaskCounterMetric
registerFailedTaskCounter = P.register . P.counter $ P.Info "BTM_failed_task_count" ""

incrementFailedTaskCounter :: (L.MonadFlow m) => FailedTaskCounterMetric -> m ()
incrementFailedTaskCounter failedTaskCounter =
  L.runIO $ P.incCounter failedTaskCounter

registerTaskDurationMetric :: IO TaskDurationMetric
registerTaskDurationMetric = P.register . P.histogram (P.Info "BTM_task_duration" "") $ P.linearBuckets 0 0.1 20

putTaskDuration :: (L.MonadFlow m) => TaskDurationMetric -> Double -> m ()
putTaskDuration taskDurationMetric duration =
  L.runIO $ P.observe taskDurationMetric duration
