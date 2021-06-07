module Utils.Metrics
  ( module Utils.Metrics,
    module CoreMetrics,
  )
where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as CoreMetrics
import Data.Time (UTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> Flow ()
incrementCaseCount caseStatus caseType = do
  caseCounter <- metricsCaseCounter <$> ask
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

putSearchDuration :: P.Histogram -> Double -> FlowR e ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics :: Text -> Flow ()
startSearchMetrics txnId = do
  searchRedisExTime <- (.metricsSearchDurationTimeout) <$> ask
  (_, failureCounter) <- metricsSearchDuration <$> ask
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

finishSearchMetrics :: Text -> Flow ()
finishSearchMetrics txnId = do
  searchRedisExTime <- (.metricsSearchDurationTimeout) <$> ask
  (searchDurationHistogram, _) <- metricsSearchDuration <$> ask
  endTime <- getCurrentTime
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
