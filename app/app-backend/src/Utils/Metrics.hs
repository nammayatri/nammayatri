module Utils.Metrics
  ( HasBAPMetrics (..),
    incrementCaseCount,
    startSearchMetrics,
    finishSearchMetrics,
    registerCaseCounter,
    registerSearchDurationHistogram,
  )
where

import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Utils.Common
import Data.Time (diffUTCTime)
import Data.Time.Clock (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

class HasBAPMetrics m where
  getCaseCounter :: m (P.Vector P.Label2 P.Counter)
  getSearchDurationHistogram :: m P.Histogram

registerCaseCounter :: IO (P.Vector P.Label2 P.Counter)
registerCaseCounter = P.register $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

incrementCaseCount :: HasBAPMetrics (FlowR e) => Case.CaseStatus -> Case.CaseType -> FlowR e ()
incrementCaseCount caseStatus caseType = do
  caseCounter <- getCaseCounter
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

registerSearchDurationHistogram :: IO P.Histogram
registerSearchDurationHistogram = P.register . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 32

putSearchDuration :: HasBAPMetrics (FlowR e) => Double -> FlowR e ()
putSearchDuration duration = do
  searchDurationHistogram <- getSearchDurationHistogram
  L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

searchRedisExTime :: (Num a) => a
searchRedisExTime = 15

startSearchMetrics :: HasBAPMetrics (FlowR e) => Text -> FlowR e ()
startSearchMetrics txnId = do
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime searchRedisExTime
  fork "Gateway Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    tryToPutSearchDuration txnId (const searchRedisExTime)

finishSearchMetrics :: HasBAPMetrics (FlowR e) => Text -> FlowR e ()
finishSearchMetrics txnId = do
  endTime <- getCurrentTime
  tryToPutSearchDuration txnId (realToFrac . diffUTCTime endTime)

tryToPutSearchDuration :: HasBAPMetrics (FlowR e) => Text -> (UTCTime -> Double) -> FlowR e ()
tryToPutSearchDuration txnId countDurr =
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration $ countDurr startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId
