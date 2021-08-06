module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Data.Time (UTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records.Extra
import Prometheus as P
import Types.Metrics (BAPMetricsContainer, HasBAPMetrics)
import qualified Types.Storage.Case as Case

startSearchMetrics :: HasBAPMetrics m r => Text -> m ()
startSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  startSearchMetrics' bmContainer txnId

finishSearchMetrics :: HasBAPMetrics m r => Text -> m ()
finishSearchMetrics txnId = do
  bmContainer <- asks (.bapMetrics)
  finishSearchMetrics' bmContainer txnId

incrementCaseCount :: HasBAPMetrics m r => Case.CaseStatus -> Case.CaseType -> m ()
incrementCaseCount caseStatus caseType = do
  bmContainer <- asks (.bapMetrics)
  incrementCaseCount' bmContainer caseStatus caseType

incrementCaseCount' :: L.MonadFlow m => BAPMetricsContainer -> Case.CaseStatus -> Case.CaseType -> m ()
incrementCaseCount' bmContainer caseStatus caseType = do
  let caseCounter = bmContainer.caseCounter
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

putSearchDuration :: L.MonadFlow m => P.Histogram -> Double -> m ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
startSearchMetrics' bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = getSecond bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime (searchRedisExTime + 1) -- a bit more time to
  -- allow forked thread to handle failure
  fork "Gateway Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.deleteKeyRedis (searchDurationKey txnId)
          L.runIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics' :: MonadFlow m => BAPMetricsContainer -> Text -> m ()
finishSearchMetrics' bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = getSecond bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
