module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P
import qualified Types.Metrics as Metric

incrementTaskCounter :: L.MonadFlow m => Metric.TaskCounterMetric -> m ()
incrementTaskCounter taskCounter =
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter :: L.MonadFlow m => Metric.FailedTaskCounterMetric -> m ()
incrementFailedTaskCounter failedTaskCounter =
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration :: L.MonadFlow m => Metric.TaskDurationMetric -> Double -> m ()
putTaskDuration taskDurationMetric duration =
  L.runIO $ P.observe taskDurationMetric duration
