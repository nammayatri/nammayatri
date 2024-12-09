module SharedLogic.KaalChakra.Chakras where

import qualified Data.Map as M
import Domain.Action.Dashboard.Management.NammaTag (kaalChakraHandle)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWCT
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator
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
    HasField "serviceClickhouseCfg" r ClickhouseCfg,
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
  let completeOldJob = Nothing -- no matter for job handler
  LYT.RunKaalChakraJobReq {usersSet = LYT.ALL_USERS, ..}

mkRunKaalChakraUpdateTagJobReq ::
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  LYT.UpdateKaalBasedTagsJobReq
mkRunKaalChakraUpdateTagJobReq chakra LYT.UpdateKaalBasedTagsData {..} = LYT.UpdateKaalBasedTagsJobReq {usersSet = LYT.ALL_USERS, ..}

runKaalChakraAndResheduleJob ::
  ChakraJobs m r c =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.RunKaalChakraJobReq ->
  m ExecutionResult
runKaalChakraAndResheduleJob merchantId merchantOperatingCityId req = do
  eventResult <- Event.kaalChakraEvent req
  now <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) now
    LYT.Completed -> do
      void $ Event.clearEventData req.chakra Nothing --- passing Nothing will only clear the batch number key, which is required for the next job from 0 again
      whenJust eventResult.eventId $ \eventId -> Event.createUpdateUserTagDataJob (kaalChakraHandle merchantId merchantOperatingCityId) req eventId (addUTCTime 60 now) -- starting updateTags job after 60 seconds of caching UserData
      pure Complete

runDailyJob ::
  ChakraJobs m r c =>
  Job 'Daily ->
  m ExecutionResult
runDailyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Daily Job"
  let req = mkRunKaalChakraJobReq LYT.Daily jobInfo.jobData
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId req

runWeeklyJob ::
  ChakraJobs m r c =>
  Job 'Weekly ->
  m ExecutionResult
runWeeklyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Weekly Job"
  let req = mkRunKaalChakraJobReq LYT.Weekly jobInfo.jobData
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId req

runQuarterlyJob ::
  ChakraJobs m r c =>
  Job 'Quarterly ->
  m ExecutionResult
runQuarterlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  let req = mkRunKaalChakraJobReq LYT.Quarterly jobInfo.jobData
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId req

runMonthlyJob ::
  ChakraJobs m r c =>
  Job 'Monthly ->
  m ExecutionResult
runMonthlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  let req = mkRunKaalChakraJobReq LYT.Monthly jobInfo.jobData
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId req

runKaalChakraUpdateTagsJob ::
  ChakraJobs m r c =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.UpdateKaalBasedTagsJobReq ->
  m ExecutionResult
runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId req = do
  eventResult <- Event.updateUserTagsHandler (kaalChakraHandle merchantId merchantOperatingCityId) req
  now <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) now
    LYT.Completed -> do
      void $ Event.clearEventData req.chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime now req.chakra
      Event.createFetchUserDataJob (kaalChakraHandle merchantId merchantOperatingCityId) req newScheduleTime
      pure Complete

-- for now keeping the rescheduling of ckahra job to be at night 12:00 + some delta based on chakra type
getNextChakraTime :: UTCTime -> LYT.Chakra -> UTCTime
getNextChakraTime now chakra = do
  let endOfDayToday = SWC.incrementPeriod SWCT.Days now
  -- above function gives us the end of day time for the next day,
  -- i.e. 00:00 of the next day no matter what the time today you run this function.

  -- adding n - 1 days below due to above.
  flip addUTCTime endOfDayToday $ case chakra of
    LYT.Daily -> 86400 + 3600 - 19800
    LYT.Weekly -> 7 * 86400 + 7200 - 19800
    LYT.Monthly -> 30 * 86400 + 10800 - 19800
    LYT.Quarterly -> 90 * 86400 + 14400 - 19800

runDailyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'DailyUpdateTag ->
  m ExecutionResult
runDailyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Daily Job"
  let req = mkRunKaalChakraUpdateTagJobReq LYT.Daily jobInfo.jobData
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId req

runWeeklyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'WeeklyUpdateTag ->
  m ExecutionResult
runWeeklyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Weekly Job"
  let req = mkRunKaalChakraUpdateTagJobReq LYT.Weekly jobInfo.jobData
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId req

runQuarterlyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'QuarterlyUpdateTag ->
  m ExecutionResult
runQuarterlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  let req = mkRunKaalChakraUpdateTagJobReq LYT.Quarterly jobInfo.jobData
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId req

runMonthlyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'MonthlyUpdateTag ->
  m ExecutionResult
runMonthlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  let req = mkRunKaalChakraUpdateTagJobReq LYT.Monthly jobInfo.jobData
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId req
