module SharedLogic.Allocator.Jobs.FleetAnalytics.RedisRecon
  ( runFleetAnalyticsRedisReconJob,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.FleetAnalytics.Realtime (reconcileFleetLiveRedis, reconcileOperatorLiveRedis, reconcileOperatorsRedis)
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetOperatorStatsExtra as QFleetOpsExtra
import qualified Storage.Queries.PersonExtra as QPerson

-- Held for one batch so two runs do not write the same keys together.
-- Released before the next batch is enqueued.
lockKey :: Text
lockKey = "fleet-analytics-redis-recon:lock"

lockTtlSeconds :: Int
lockTtlSeconds = 300

defaultPageSize :: Int
defaultPageSize = 50

-- Pause between batches of one pass. intervalSeconds on the job is the pause after a finished pass.
chunkDelaySeconds :: Int
chunkDelaySeconds = 5

runFleetAnalyticsRedisReconJob ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Job 'FleetAnalyticsRedisRecon ->
  m ExecutionResult
runFleetAnalyticsRedisReconJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      pageSize = max 1 (fromMaybe defaultPageSize jobData.pageSize)
  locked <- Redis.setNxExpire lockKey lockTtlSeconds ("1" :: Text)
  if not locked
    then do
      logInfo "fleet analytics recon skipped, another run holds the lock"
      enqueue chunkDelaySeconds jobData
      pure Complete
    else do
      outcome <- try @_ @SomeException $ runBatch jobData.cursor pageSize jobData.reconLiveCounters
      void $ Redis.del lockKey
      case outcome of
        Left err -> do
          enqueue chunkDelaySeconds jobData
          throwM err
        Right (checked, drifted, mbNextCursor) -> do
          let done = isNothing mbNextCursor
          logInfo $
            "fleet analytics recon batch checked="
              <> show checked
              <> " drifted="
              <> show drifted
              <> " cursor="
              <> show jobData.cursor
              <> " nextCursor="
              <> show mbNextCursor
              <> " passDone="
              <> show done
              <> " liveCounters="
              <> show jobData.reconLiveCounters
          enqueue
            (if done then jobData.intervalSeconds else chunkDelaySeconds)
            jobData {cursor = mbNextCursor}
          pure Complete
  where
    enqueue delay jobData =
      createJobIn @_ @'FleetAnalyticsRedisRecon Nothing Nothing (secondsToNominalDiffTime $ Seconds delay) jobData

-- One page of fleet_operator_stats. Operators always get the six counters. Live gauges run
-- only when reconLiveCounters is set. Nothing means this pass is finished.
runBatch ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Maybe Text ->
  Int ->
  Bool ->
  m (Int, Int, Maybe Text)
runBatch cursor pageSize reconLiveCounters = do
  rows <- QFleetOpsExtra.findStatsPageAfter cursor pageSize
  let ids = map (.fleetOperatorId) rows
  people <- if null ids then pure [] else QPerson.findAllByPersonIds ids
  let operatorPeople = [person | person <- people, person.role == DP.OPERATOR]
      operatorIds = map (.id.getId) operatorPeople
      fleetOwnerIds = [person.id.getId | person <- people, person.role == DP.FLEET_OWNER]
  kvStats <- if null operatorIds then pure [] else QFleetOpsExtra.findAllByFleetOperatorIds operatorIds
  drifted <- reconcileOperatorsRedis kvStats
  liveDrifted <-
    if reconLiveCounters
      then do
        operatorLiveInputs <- catMaybes <$> traverse operatorLiveInput operatorPeople
        operatorLiveDrifted <- reconcileOperatorLiveRedis operatorLiveInputs
        fleetLiveDrifted <- reconcileFleetLiveRedis fleetOwnerIds
        pure (operatorLiveDrifted + fleetLiveDrifted)
      else pure 0
  let mbNextCursor =
        if length rows < pageSize
          then Nothing
          else case reverse ids of
            (lastId : _) -> Just lastId
            [] -> Nothing
      checked = length kvStats + if reconLiveCounters then length fleetOwnerIds else 0
  pure (checked, drifted + liveDrifted, mbNextCursor)
  where
    operatorLiveInput person = do
      mbCfg <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
      case mbCfg of
        Nothing -> do
          logError $ "fleet analytics recon skipped operator live counts, transporter config missing operator=" <> person.id.getId
          pure Nothing
        Just cfg -> pure $ Just (person.id.getId, cfg.analyticsConfig.useDbForEarningAndMetrics)
