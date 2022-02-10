module Beckn.Tools.Metrics
  ( module CoreMetrics,
    module Beckn.Tools.Metrics,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Tools.Metrics.Types (BAPMetricsContainer, HasBAPMetrics)
import Beckn.Types.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Data.Time (diffUTCTime)
import GHC.Records.Extra
import Prometheus as P

startSearchMetrics :: HasBAPMetrics m r => Text -> m ()
startSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  startSearchMetrics' bmContainer txnId

finishSearchMetrics :: HasBAPMetrics m r => Text -> m ()
finishSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  finishSearchMetrics' bmContainer txnId

incrementSearchRequestCount :: HasBAPMetrics m r => m ()
incrementSearchRequestCount = do
  bmContainer <- asks (.bapMetrics)
  incrementCaseCount' bmContainer

incrementCaseCount' :: MonadIO m => BAPMetricsContainer -> m ()
incrementCaseCount' bmContainer = do
  let searchRequestCounter = bmContainer.searchRequestCounter
  liftIO $ P.incCounter searchRequestCounter --is it correct that Euler.runIO = liftIO?

putSearchDuration :: MonadIO m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = liftIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
startSearchMetrics' bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.deleteKeyRedis (searchDurationKey txnId)
          liftIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
finishSearchMetrics' bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
