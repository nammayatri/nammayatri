module Tools.Metrics.BAPMetrics
  ( module Tools.Metrics.BAPMetrics,
    module Reexport,
  )
where

import Data.Time (diffUTCTime)
import GHC.Records.Extra
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Prometheus as P
import Tools.Metrics.BAPMetrics.Types as Reexport

startSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> Text -> m ()
startSearchMetrics merchantName txnId = do
  bmContainer <- asks (.bapMetrics)
  startSearchMetrics' bmContainer merchantName txnId

finishSearchMetrics :: (Redis.HedisFlow m r, HasBAPMetrics m r) => Text -> Text -> m ()
finishSearchMetrics merchantName txnId = do
  bmContainer <- asks (.bapMetrics)
  finishSearchMetrics' bmContainer merchantName txnId

incrementSearchRequestCount :: HasBAPMetrics m r => Text -> m ()
incrementSearchRequestCount merchantName = do
  bmContainer <- asks (.bapMetrics)
  incrementSearchRequestCount' bmContainer merchantName

incrementSearchRequestCount' :: MonadIO m => BAPMetricsContainer -> Text -> m ()
incrementSearchRequestCount' bmContainer merchantName = do
  let searchRequestCounter = bmContainer.searchRequestCounter
  liftIO $ P.withLabel searchRequestCounter merchantName P.incCounter

putSearchDuration :: MonadIO m => P.Vector P.Label1 P.Histogram -> Text -> Double -> m ()
putSearchDuration searchDurationHistogram merchantName duration = liftIO $ P.withLabel searchDurationHistogram merchantName (`P.observe` duration)

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: (Redis.HedisFlow m r, MonadFlow m, MonadMask m) => BAPMetricsContainer -> Text -> Text -> m ()
startSearchMetrics' bmContainer merchantName txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExp (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    liftIO $ threadDelay $ searchRedisExTime * 1000000
    Redis.whenWithLockRedis (searchDurationLockKey txnId) searchRedisExTime $ do
      Redis.get (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.del (searchDurationKey txnId)
          liftIO $ P.withLabel failureCounter merchantName P.incCounter
        Nothing -> return ()

finishSearchMetrics' :: (Redis.HedisFlow m r, MonadTime m, MonadMask m) => BAPMetricsContainer -> Text -> Text -> m ()
finishSearchMetrics' bmContainer merchantName txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSeconds bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  Redis.whenWithLockRedis (searchDurationLockKey txnId) searchRedisExTime $ do
    Redis.get (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.del (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram merchantName . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
