module Utils.Metrics
  ( incrementCaseCount,
    startSearchMetrics,
    finishSearchMetrics,
  )
where

import App.Types
import qualified Beckn.Storage.Redis.Queries as Redis
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import Beckn.Utils.Common
import Data.Time (diffUTCTime)
import Data.Time.Clock (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

caseCounter :: P.Vector P.Label2 P.Counter
caseCounter = P.unsafeRegister $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> Flow ()
incrementCaseCount caseStatus caseType =
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

searchDurationHistogram :: P.Histogram
searchDurationHistogram = P.unsafeRegister . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 32

putSearchDuration :: Double -> Flow ()
putSearchDuration duration =
  L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_search:received"

searchDurationLockKey :: Text -> Text
searchDurationLockKey txnId = txnId <> ":on_search"

searchRedisExTime :: (Num a) => a
searchRedisExTime = 15

startSearchMetrics :: Text -> Flow ()
startSearchMetrics txnId = do
  startTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) startTime searchRedisExTime
  fork "Gateway Search Metrics" $ do
    L.runIO $ threadDelay $ searchRedisExTime * 1000000
    tryToPutSearchDuration txnId (const searchRedisExTime)

finishSearchMetrics :: Text -> Flow ()
finishSearchMetrics txnId = do
  endTime <- getCurrentTime
  tryToPutSearchDuration txnId (realToFrac . diffUTCTime endTime)

tryToPutSearchDuration :: Text -> (UTCTime -> Double) -> Flow ()
tryToPutSearchDuration txnId countDurr =
  whenM (Redis.tryLockRedis (searchDurationLockKey txnId) searchRedisExTime) $ do
    Redis.getKeyRedis (searchDurationKey txnId) >>= \case
      Just startTime -> do
        void $ Redis.deleteKeyRedis (searchDurationKey txnId)
        putSearchDuration $ countDurr startTime
      Nothing -> return ()
    Redis.unlockRedis $ searchDurationLockKey txnId