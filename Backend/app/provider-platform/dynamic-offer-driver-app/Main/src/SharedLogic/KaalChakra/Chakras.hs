module SharedLogic.KaalChakra.Chakras where

import qualified Data.Map as M
import Domain.Action.Dashboard.Management.NammaTag (kaalChakraHandle)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator
import qualified SharedLogic.KaalChakra.Utils as Time
import Storage.Beam.SchedulerJob ()
import qualified Tools.Metrics as Metrics

type ChakraJobs m r c =
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "serviceClickhouseEnv" r ClickhouseEnv,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r
  )

mkRunKaalChakraJobReq ::
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  LYT.RunKaalChakraJobReq
mkRunKaalChakraJobReq chakra LYT.KaalChakraJobData {..} = do
  let action = LYT.RUN -- no matter for job handler
  LYT.RunKaalChakraJobReq {usersSet = LYT.ALL_USERS, ..}

runDailyJob ::
  ChakraJobs m r c =>
  Job 'Daily ->
  m ExecutionResult
runDailyJob Job {id, jobInfo, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Daily Job"
  let req = mkRunKaalChakraJobReq LYT.Daily jobInfo.jobData
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementDay scheduledAt

runWeeklyJob ::
  ChakraJobs m r c =>
  Job 'Weekly ->
  m ExecutionResult
runWeeklyJob Job {id, jobInfo, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Weekly Job"
  let req = mkRunKaalChakraJobReq LYT.Weekly jobInfo.jobData
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementWeek scheduledAt

runQuarterlyJob ::
  ChakraJobs m r c =>
  Job 'Quarterly ->
  m ExecutionResult
runQuarterlyJob Job {id, jobInfo, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  let req = mkRunKaalChakraJobReq LYT.Quarterly jobInfo.jobData
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementQuarter scheduledAt

runMonthlyJob ::
  ChakraJobs m r c =>
  Job 'Monthly ->
  m ExecutionResult
runMonthlyJob Job {id, jobInfo, scheduledAt} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  let req = mkRunKaalChakraJobReq LYT.Monthly jobInfo.jobData
  void $ Event.kaalChakraEvent kaalChakraHandle req
  pure $ ReSchedule $ Time.incrementMonth scheduledAt
