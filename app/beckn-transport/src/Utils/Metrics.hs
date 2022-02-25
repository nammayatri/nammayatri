module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Common (Milliseconds, getSeconds)
import Beckn.Types.Id
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Beckn.Utils.Time (getClockTimeInMs)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import qualified Types.Metrics as Metric
import Types.Storage.Organization
import Utils.Common (Forkable (fork), MonadFlow)

incrementTaskCounter :: Metric.HasAllocatorMetrics m r => m ()
incrementTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementTaskCounter' bmContainer

incrementFailedTaskCounter :: Metric.HasAllocatorMetrics m r => m ()
incrementFailedTaskCounter = do
  bmContainer <- asks (.btmMetrics)
  incrementFailedTaskCounter' bmContainer

putTaskDuration :: Metric.HasAllocatorMetrics m r => Milliseconds -> m ()
putTaskDuration duration = do
  bmContainer <- asks (.btmMetrics)
  putTaskDuration' bmContainer duration

incrementTaskCounter' :: L.MonadFlow m => Metric.AllocatorMetricsContainer -> m ()
incrementTaskCounter' bmContainer = do
  let taskCounter = bmContainer.taskCounter
  L.runIO $ P.incCounter taskCounter

incrementFailedTaskCounter' :: L.MonadFlow m => Metric.AllocatorMetricsContainer -> m ()
incrementFailedTaskCounter' bmContainer = do
  let failedTaskCounter = bmContainer.failedTaskCounter
  L.runIO $ P.incCounter failedTaskCounter

putTaskDuration' :: L.MonadFlow m => Metric.AllocatorMetricsContainer -> Milliseconds -> m ()
putTaskDuration' bmContainer duration = do
  let taskDuration = bmContainer.taskDuration
  L.runIO $ P.observe taskDuration . (/ 1000) . fromIntegral $ duration

putFareAndDistanceDeviations :: (MonadMonitor m, Metric.HasTransporterMetrics m r) => Amount -> Double -> m ()
putFareAndDistanceDeviations fareDiff distanceDiff = do
  Metric.TransporterMetricsContainer {..} <- asks (.transporterMetrics)
  P.observe realFareDeviation $ amountToDouble fareDiff
  P.observe realDistanceDeviation distanceDiff

type SearchMetricsMVar = MVar Milliseconds

startSearchMetrics :: Metric.HasBPPMetrics m r => Id Organization -> m SearchMetricsMVar
startSearchMetrics transporterId = do
  bmContainer <- asks (.bppMetrics)
  startSearchMetrics' transporterId bmContainer

finishSearchMetrics :: Metric.HasBPPMetrics m r => Id Organization -> SearchMetricsMVar -> m ()
finishSearchMetrics transporterId searchMetricsMVar = do
  bmContainer <- asks (.bppMetrics)
  finishSearchMetrics' transporterId bmContainer searchMetricsMVar

putSearchDuration :: L.MonadFlow m => Id Organization -> P.Vector P.Label1 P.Histogram -> Double -> m ()
putSearchDuration transporterId searchDurationHistogram duration =
  L.runIO $
    P.withLabel
      searchDurationHistogram
      (show transporterId)
      (`P.observe` duration)

startSearchMetrics' :: MonadFlow m => Id Organization -> Metric.BPPMetricsContainer -> m SearchMetricsMVar
startSearchMetrics' transporterId bmContainer = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchDurationTimeout = getSeconds bmContainer.searchDurationTimeout
  startTime <- getClockTimeInMs
  searchMetricsMVar <- liftIO $ newMVar startTime
  fork "BPP Search Metrics" $ do
    liftIO $ threadDelay $ searchDurationTimeout * 1000000
    whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \_ -> do
      liftIO $ P.withLabel failureCounter (show transporterId) P.incCounter
  return searchMetricsMVar

finishSearchMetrics' ::
  MonadFlow m =>
  Id Organization ->
  Metric.BPPMetricsContainer ->
  SearchMetricsMVar ->
  m ()
finishSearchMetrics' transporterId bmContainer searchMetricsMVar = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
  whenJustM (liftIO $ tryTakeMVar searchMetricsMVar) $ \startTime -> do
    endTime <- getClockTimeInMs
    putSearchDuration transporterId searchDurationHistogram $ fromIntegral $ endTime - startTime
