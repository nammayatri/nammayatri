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

class HasBAPMetrics m where
  incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> m ()
  startSearchMetrics :: Text -> m ()
  finishSearchMetrics :: Text -> m ()

incrementCaseCount' :: L.MonadFlow m => CaseCounterMetric -> Case.CaseStatus -> Case.CaseType -> m ()
incrementCaseCount' caseCounter caseStatus caseType = L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

putSearchDuration :: P.Histogram -> Double -> FlowR e ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics' :: SearchDurationMetric -> Int -> Text -> FlowR r ()
startSearchMetrics' (_, failureCounter) searchRedisExTime txnId = do
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

finishSearchMetrics' :: SearchDurationMetric -> Int -> Text -> FlowR r ()
finishSearchMetrics' (searchDurationHistogram, _) searchRedisExTime txnId = do
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
