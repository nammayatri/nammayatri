module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Flow
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import qualified Types.Metrics as Metric

type HasBTMMetrics r = HasField "btmMetrics" r Metric.BTMMetricsContainer

incrementTaskCounterFlow :: HasBTMMetrics r => FlowR r ()
incrementTaskCounterFlow = do
  bmContainer <- asks (.btmMetrics)
  incrementTaskCounter bmContainer

incrementFailedTaskCounterFlow :: HasBTMMetrics r => FlowR r ()
incrementFailedTaskCounterFlow = do
  bmContainer <- asks (.btmMetrics)
  incrementFailedTaskCounter bmContainer

incrementTaskCounter :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementTaskCounter bmContainer = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter :: L.MonadFlow m => Metric.BTMMetricsContainer -> m ()
incrementFailedTaskCounter bmContainer = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

addTaskDurationFlow ::
  ( HasBTMMetrics r,
    MonadReader r m,
    L.MonadFlow m
  ) =>
  Double ->
  () ->
  m ()
addTaskDurationFlow duration _ = do
  bmContainer <- asks (.btmMetrics)
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.observe taskDuration duration
