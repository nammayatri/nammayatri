module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import Beckn.Types.Amount
import Beckn.Types.Common (Milliseconds, getSeconds)
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Data.Time (UTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import qualified Types.Metrics as Metric
import Utils.Common (Forkable (fork), MonadFlow, MonadTime (getCurrentTime))

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
  
startSearchMetrics :: Metric.HasBPPMetrics m r => Text -> m ()
startSearchMetrics txnId = do
  bmContainer <- asks (.bppMetrics)
  startSearchMetrics' bmContainer txnId

finishSearchMetrics :: Metric.HasBPPMetrics m r => Text -> m ()
finishSearchMetrics txnId = do
  bmContainer <- asks (.bppMetrics)
  finishSearchMetrics' bmContainer txnId

putSearchDuration :: L.MonadFlow m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "bpp:" <> txnId <> ":searchDuration:start"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":searchDuration:locker"

startSearchMetrics' :: MonadFlow m => Metric.BPPMetricsContainer -> Text -> m ()
startSearchMetrics' bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "BPP Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.deleteKeyRedis (searchDurationKey txnId)
          L.runIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics' :: MonadFlow m => Metric.BPPMetricsContainer -> Text -> m ()
finishSearchMetrics' bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac $ diffUTCTime endTime startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId