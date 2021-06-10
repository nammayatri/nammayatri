module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P
import qualified Types.Metrics as Metric

incrementTaskCounter :: Metric.TaskCounterMetric -> FlowR k ()
incrementTaskCounter taskCounter =
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter :: Metric.FailedTaskCounterMetric -> FlowR k ()
incrementFailedTaskCounter failedTaskCounter =
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration :: Metric.TaskDurationMetric -> Double -> FlowR k ()
putTaskDuration taskDurationMetric duration =
  L.runIO $ P.observe taskDurationMetric duration
