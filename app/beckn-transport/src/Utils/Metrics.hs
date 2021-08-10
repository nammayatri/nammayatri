module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Common (Milliseconds)
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import qualified Types.Metrics as Metric

incrementTaskCounter :: Metric.HasBTMMetrics m r => m ()
incrementTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementTaskCounter' bmContainer

incrementFailedTaskCounter :: Metric.HasBTMMetrics m r => m ()
incrementFailedTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementFailedTaskCounter' bmContainer

putTaskDuration :: Metric.HasBTMMetrics m r => Milliseconds -> m ()
putTaskDuration duration = do
  bmContainer <- asks (.btmMetrics)
  putTaskDuration' bmContainer duration

incrementTaskCounter' :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementTaskCounter' bmContainer = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter' :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementFailedTaskCounter' bmContainer = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration' :: L.MonadFlow m => Metric.BTMMetricsContainer -> Milliseconds -> m ()
putTaskDuration' bmContainer duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.observe taskDuration . (/ 1000) . fromIntegral $ duration
