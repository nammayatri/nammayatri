module SharedLogic.KaalChakra.Chakras where

import qualified Data.Map as M
import Domain.Action.Dashboard.Management.NammaTag.Handle (kaalChakraHandle)
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
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
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
  UTCTime ->
  LYT.RunKaalChakraJobReq
mkRunKaalChakraJobReq chakra LYT.KaalChakraJobData {..} now = do
  let action = LYT.SCHEDULE now -- error handle is different for RUN and SCHEDULE cases
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
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  m ExecutionResult
runKaalChakraAndResheduleJob merchantId merchantOperatingCityId chakra jobData = do
  now <- getCurrentTime
  let req = mkRunKaalChakraJobReq chakra jobData now
  eventResult <- Event.kaalChakraEvent req
  timeAfterRun <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) timeAfterRun
    LYT.Completed -> do
      void $ Event.clearEventData chakra Nothing --- passing Nothing will only clear the batch number key, which is required for the next job from 0 again
      whenJust eventResult.eventId $ \eventId -> do
        let updateUserTagJobData = LYT.mkUpdateTagDataFromKaalChakraJobData req eventId
        createUpdateUserTagDataJob merchantId merchantOperatingCityId chakra updateUserTagJobData (addUTCTime 60 timeAfterRun) -- starting updateTags job after 60 seconds of caching UserData
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime timeAfterRun chakra
      createFetchUserDataJob merchantId merchantOperatingCityId chakra jobData newScheduleTime
      pure $ Terminate "Fetch user data job failed"

runDailyJob ::
  ChakraJobs m r c =>
  Job 'Daily ->
  m ExecutionResult
runDailyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Daily Job"
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId LYT.Daily jobInfo.jobData

runWeeklyJob ::
  ChakraJobs m r c =>
  Job 'Weekly ->
  m ExecutionResult
runWeeklyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Weekly Job"
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId LYT.Weekly jobInfo.jobData

runQuarterlyJob ::
  ChakraJobs m r c =>
  Job 'Quarterly ->
  m ExecutionResult
runQuarterlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId LYT.Quarterly jobInfo.jobData

runMonthlyJob ::
  ChakraJobs m r c =>
  Job 'Monthly ->
  m ExecutionResult
runMonthlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  runKaalChakraAndResheduleJob merchantId merchantOperatingCityId LYT.Monthly jobInfo.jobData

runKaalChakraUpdateTagsJob ::
  ChakraJobs m r c =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  m ExecutionResult
runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId chakra jobData = do
  let req = mkRunKaalChakraUpdateTagJobReq chakra jobData
  eventResult <- Event.updateUserTagsHandler kaalChakraHandle req
  now <- getCurrentTime
  case eventResult.chakraBatchState of
    LYT.Continue shortDelayTime -> do
      pure $ ReSchedule $ addUTCTime (fromIntegral shortDelayTime) now
    LYT.Completed -> do
      void $ Event.clearEventData req.chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime now req.chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      createFetchUserDataJob merchantId merchantOperatingCityId chakra fetchUserDataJobData newScheduleTime
      pure Complete
    LYT.Failed -> do
      -- clear all event data and create new job for the next cycle
      void $ Event.clearEventData chakra eventResult.eventId
      let newScheduleTime = getNextChakraTime now chakra
      let fetchUserDataJobData = LYT.mkKaalChakraJobDataFromUpdateTagData req True
      createFetchUserDataJob merchantId merchantOperatingCityId chakra fetchUserDataJobData newScheduleTime
      pure $ Terminate "Update user tags job failed"

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
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId LYT.Daily jobInfo.jobData

runWeeklyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'WeeklyUpdateTag ->
  m ExecutionResult
runWeeklyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Weekly Job"
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId LYT.Weekly jobInfo.jobData

runQuarterlyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'QuarterlyUpdateTag ->
  m ExecutionResult
runQuarterlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Quarterly Job"
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId LYT.Quarterly jobInfo.jobData

runMonthlyUpdateTagJob ::
  ChakraJobs m r c =>
  Job 'MonthlyUpdateTag ->
  m ExecutionResult
runMonthlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  logInfo "Running Monthly Job"
  runKaalChakraUpdateTagsJob merchantId merchantOperatingCityId LYT.Monthly jobInfo.jobData

createFetchUserDataJob ::
  ChakraJobs m r c =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  UTCTime ->
  m ()
createFetchUserDataJob merchantId merchantOperatingCityId chakra jobData scheduledTime = case chakra of
  LYT.Daily -> QAllJ.createJobByTime @_ @'Daily merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Weekly -> QAllJ.createJobByTime @_ @'Weekly merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Monthly -> QAllJ.createJobByTime @_ @'Monthly merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Quarterly -> QAllJ.createJobByTime @_ @'Quarterly merchantId merchantOperatingCityId scheduledTime jobData

createUpdateUserTagDataJob ::
  ChakraJobs m r c =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  UTCTime ->
  m ()
createUpdateUserTagDataJob merchantId merchantOperatingCityId chakra jobData scheduledTime = case chakra of
  LYT.Daily -> QAllJ.createJobByTime @_ @'DailyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Weekly -> QAllJ.createJobByTime @_ @'WeeklyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Monthly -> QAllJ.createJobByTime @_ @'MonthlyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
  LYT.Quarterly -> QAllJ.createJobByTime @_ @'QuarterlyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
