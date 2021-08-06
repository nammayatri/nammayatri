module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Common (Milliseconds, getSeconds)
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Beckn.Utils.Time (getClockTimeInMs)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import qualified Types.Metrics as Metric
import Utils.Common (Forkable (fork), MonadFlow)

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

putFareAndDistanceDeviations :: (MonadMonitor m, Metric.HasTransporterMetrics m r) => Amount -> Double -> m ()
putFareAndDistanceDeviations fareDiff distanceDiff = do
  Metric.TransporterMetricsContainer {..} <- asks (.transporterMetrics)
  P.observe realFareDeviation $ amountToDouble fareDiff
  P.observe realDistanceDeviation distanceDiff

type SearchMetricsMVar = MVar Milliseconds

startSearchMetrics :: Metric.HasBPPMetrics m r => m SearchMetricsMVar
startSearchMetrics = do
  bmContainer <- asks (.bppMetrics)
  startSearchMetrics' bmContainer

finishSearchMetrics :: Metric.HasBPPMetrics m r => SearchMetricsMVar -> m ()
finishSearchMetrics searchMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  finishSearchMetrics' bmContainer searchMetricsMVar

putSearchDuration :: L.MonadFlow m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

startSearchMetrics' :: MonadFlow m => Metric.BPPMetricsContainer -> m SearchMetricsMVar
startSearchMetrics' bmContainer = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getClockTimeInMs
  searchMetricsMVar <- L.runIO $ newMVar startTime
  fork "BPP Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    whenJustM (L.runIO $ tryTakeMVar searchMetricsMVar) $ \_ -> do
      L.runIO $ P.incCounter failureCounter
  return searchMetricsMVar

finishSearchMetrics' :: MonadFlow m => Metric.BPPMetricsContainer -> SearchMetricsMVar -> m ()
finishSearchMetrics' bmContainer searchMetricsMVar = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
  whenJustM (L.runIO $ tryTakeMVar searchMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putSearchDuration searchDurationHistogram . fromIntegral $ endTime - startTime
