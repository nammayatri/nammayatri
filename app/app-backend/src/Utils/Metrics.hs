module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Data.Time (UTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P
import Types.Metrics

incrementCaseCount :: L.MonadFlow m => BAPMetricsContainer -> Case.CaseStatus -> Case.CaseType -> m ()
incrementCaseCount bmContainer caseStatus caseType = do
  let caseCounter = bmContainer.caseCounter
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

putSearchDuration :: P.Histogram -> Double -> FlowR e ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics :: BAPMetricsContainer -> Text -> FlowR k ()
startSearchMetrics bmContainer txnId = do
  let (_, failureCounter) = bmContainer.searchDuration
      searchRedisExTime = bmContainer.searchDurationTimeout
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime searchRedisExTime
  fork "Gateway Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Just (_ :: UTCTime) -> do
          void $ Redis.deleteKeyRedis (searchDurationKey txnId)
          L.runIO $ P.incCounter failureCounter
        Nothing -> return ()
      Redis.unlockRedis $ searchDurationLockKey txnId

finishSearchMetrics :: BAPMetricsContainer -> Text -> FlowR k ()
finishSearchMetrics bmContainer txnId = do
  let (searchDurationHistogram, _) = bmContainer.searchDuration
      searchRedisExTime = bmContainer.searchDurationTimeout
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
