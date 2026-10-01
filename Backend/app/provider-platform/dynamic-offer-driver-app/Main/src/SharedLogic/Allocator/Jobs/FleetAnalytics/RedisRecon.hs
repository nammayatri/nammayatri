module SharedLogic.Allocator.Jobs.FleetAnalytics.RedisRecon
  ( runFleetAnalyticsRedisReconJob,
  )
where

import qualified Data.Map as M
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.FleetAnalytics.Realtime (reconcileOperatorRedis)
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FleetOperatorStatsExtra as QFleetOps

-- Held for the scan so two runs do not write the same keys together.
-- Released before the next run is enqueued.
lockKey :: Text
lockKey = "fleet-analytics-redis-recon:lock"

lockTtlSeconds :: Int
lockTtlSeconds = 300

runFleetAnalyticsRedisReconJob ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'FleetAnalyticsRedisRecon ->
  m ExecutionResult
runFleetAnalyticsRedisReconJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let interval = jobInfo.jobData.intervalSeconds
  locked <- Redis.setNxExpire lockKey lockTtlSeconds ("1" :: Text)
  if not locked
    then do
      logInfo "fleet analytics recon skipped, another run holds the lock"
      pure Complete
    else do
      outcome <- try @_ @SomeException $ do
        rows <- QFleetOps.findAllFleetOperatorStats
        drifted <- sum <$> traverse (\row -> reconcileOperatorRedis row.fleetOperatorId) rows
        pure (length rows, drifted)
      void $ Redis.del lockKey
      case outcome of
        Left err -> throwM err
        Right (checked, drifted) -> do
          logInfo $
            "fleet analytics recon done checked="
              <> show checked
              <> " drifted="
              <> show drifted
              <> " intervalSeconds="
              <> show interval
          createJobIn @_ @'FleetAnalyticsRedisRecon Nothing Nothing (secondsToNominalDiffTime $ Seconds interval) jobInfo.jobData
          pure Complete
