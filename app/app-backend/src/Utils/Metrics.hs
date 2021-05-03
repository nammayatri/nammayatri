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
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

caseCounter :: P.Vector P.Label2 P.Counter
caseCounter = P.unsafeRegister $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> Flow ()
incrementCaseCount caseStatus caseType =
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter

searchDurationHistogram :: P.Histogram
searchDurationHistogram = P.unsafeRegister . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 30

putSearchDuration :: Double -> Flow ()
putSearchDuration duration =
  L.runIO $ P.observe searchDurationHistogram duration

searchDurationKey :: Text -> Text
searchDurationKey txnId = "beckn:" <> txnId <> ":on_seach:received"

startSearchMetrics :: Text -> Flow ()
startSearchMetrics txnId = do
  startTime <- getCurrentTime
  fork "Gateway Search Metrics" $ do
    searchDuration <- checkForKey startTime
    putSearchDuration $ realToFrac searchDuration
  where
    checkForKey startTime = do
      L.runIO $ threadDelay 200000
      Redis.getKeyRedis (searchDurationKey txnId) >>= \case
        Nothing -> do
          currTime <- getCurrentTime
          let diffTime = diffUTCTime currTime startTime
          if diffTime > 10
            then return diffTime
            else checkForKey startTime
        Just endTime -> return $ diffUTCTime endTime startTime

finishSearchMetrics :: Text -> Flow ()
finishSearchMetrics txnId = do
  endTime <- getCurrentTime
  Redis.setExRedis (searchDurationKey txnId) endTime 10
