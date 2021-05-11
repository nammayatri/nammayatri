module Utils.Metrics
  ( CaseCounterMetric,
    SearchDurationMetric,
    HasBAPMetrics (..),
    incrementCaseCount,
    startSearchMetrics,
    finishSearchMetrics,
    registerCaseCounter,
    registerSearchDurationMetric,
  )
where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Utils.Common
import Data.Time (UTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

type CaseCounterMetric = P.Vector P.Label2 P.Counter

type SearchDurationMetric = (P.Histogram, P.Counter)

class HasBAPMetrics m where
  getCaseCounterMetric :: m CaseCounterMetric
  getSearchDurationTimeout :: m Int
  getSearchDurationMetric :: m SearchDurationMetric

registerCaseCounter :: IO CaseCounterMetric
registerCaseCounter = P.register $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

incrementCaseCount :: HasBAPMetrics (FlowR e) => Case.CaseStatus -> Case.CaseType -> FlowR e ()
incrementCaseCount caseStatus caseType = do
  caseCounter <- getCaseCounterMetric
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

registerSearchDurationMetric :: Int -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)

putSearchDuration :: P.Histogram -> Double -> FlowR e ()
putSearchDuration searchDurationHistogram duration = L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

startSearchMetrics :: HasBAPMetrics (FlowR e) => Text -> FlowR e ()
startSearchMetrics txnId = do
  startTime <- getCurrentTime
  searchRedisExTime <- getSearchDurationTimeout
  (_, failureCounter) <- getSearchDurationMetric
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

finishSearchMetrics :: HasBAPMetrics (FlowR e) => Text -> FlowR e ()
finishSearchMetrics txnId = do
  endTime <- getCurrentTime
  searchRedisExTime <- getSearchDurationTimeout
  (searchDurationHistogram, _) <- getSearchDurationMetric
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration searchDurationHistogram . realToFrac . diffUTCTime endTime $ startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId