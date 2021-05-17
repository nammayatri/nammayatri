module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import qualified App.BackgroundTaskManager.Types as BTMTypes
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

incrementTaskCounter :: BTMTypes.Flow ()
incrementTaskCounter = do
  taskCounter <- asks BTMTypes.metricsBTMTaskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter :: BTMTypes.Flow ()
incrementFailedTaskCounter = do
  failedTaskCounter <- asks BTMTypes.metricsBTMFailedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration :: Double -> BTMTypes.Flow ()
putTaskDuration duration = do
  taskDurationMetric <- asks BTMTypes.metricsBTMTaskDuration
  L.runIO $ P.observe taskDurationMetric duration
