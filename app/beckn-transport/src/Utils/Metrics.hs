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

incrementTaskCounter :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementTaskCounter bmContainer = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementFailedTaskCounter bmContainer = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration :: L.MonadFlow m => Metric.BTMMetricsContainer -> Double -> m ()
putTaskDuration bmContainer duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.observe taskDuration duration
