module SharedLogic.Scheduler.Jobs.Chakras
  ( mkKaalChakraHandle,
    runDailyJob,
    runWeeklyJob,
    runQuarterlyJob,
    runMonthlyJob,
    runDailyUpdateTagJob,
    runWeeklyUpdateTagJob,
    runQuarterlyUpdateTagJob,
    runMonthlyUpdateTagJob,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import qualified Lib.Yudhishthira.Event.KaalChakra as Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.JobScheduler
import qualified SharedLogic.KaalChakra.Actions as Actions
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.Person as QPerson

type ChakraJobs m r =
  ( EsqDBFlow m r,
    CacheFlow m r,
    JobCreator r m,
    HasJobInfoMap r,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  )

mkKaalChakraHandle ::
  ChakraJobs m r =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Event.Handle m Actions.Action
mkKaalChakraHandle merchantId merchantOperatingCityId =
  Event.Handle
    { getUserTags = \userId -> do
        mbRider <- QPerson.findById $ cast @LYT.User @DPerson.Person userId
        pure $ mbRider <&> (\rider -> fromMaybe [] rider.customerNammaTags),
      updateUserTags = \userId customerTags -> QPerson.updateCustomerTags (Just customerTags) (cast @LYT.User @DPerson.Person userId),
      action = Actions.kaalChakraAction . cast @LYT.User @DPerson.Person,
      createFetchUserDataJob = createFetchUserDataJob merchantId merchantOperatingCityId,
      createUpdateUserTagDataJob = createUpdateUserTagDataJob merchantId merchantOperatingCityId
    }

runDailyJob ::
  ChakraJobs m r =>
  Job 'Daily ->
  m ExecutionResult
runDailyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraAndRescheduleJob kaalChakraHandle LYT.Daily jobInfo.jobData

runWeeklyJob ::
  ChakraJobs m r =>
  Job 'Weekly ->
  m ExecutionResult
runWeeklyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraAndRescheduleJob kaalChakraHandle LYT.Weekly jobInfo.jobData

runQuarterlyJob ::
  ChakraJobs m r =>
  Job 'Quarterly ->
  m ExecutionResult
runQuarterlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraAndRescheduleJob kaalChakraHandle LYT.Quarterly jobInfo.jobData

runMonthlyJob ::
  ChakraJobs m r =>
  Job 'Monthly ->
  m ExecutionResult
runMonthlyJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraAndRescheduleJob kaalChakraHandle LYT.Monthly jobInfo.jobData

runDailyUpdateTagJob ::
  ChakraJobs m r =>
  Job 'DailyUpdateTag ->
  m ExecutionResult
runDailyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraUpdateTagsJob kaalChakraHandle LYT.Daily jobInfo.jobData

runWeeklyUpdateTagJob ::
  ChakraJobs m r =>
  Job 'WeeklyUpdateTag ->
  m ExecutionResult
runWeeklyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraUpdateTagsJob kaalChakraHandle LYT.Weekly jobInfo.jobData

runQuarterlyUpdateTagJob ::
  ChakraJobs m r =>
  Job 'QuarterlyUpdateTag ->
  m ExecutionResult
runQuarterlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraUpdateTagsJob kaalChakraHandle LYT.Quarterly jobInfo.jobData

runMonthlyUpdateTagJob ::
  ChakraJobs m r =>
  Job 'MonthlyUpdateTag ->
  m ExecutionResult
runMonthlyUpdateTagJob Job {id, jobInfo, merchantId, merchantOperatingCityId} = withLogTag ("JobId-" <> id.getId) do
  let kaalChakraHandle = mkKaalChakraHandle merchantId merchantOperatingCityId
  Event.runKaalChakraUpdateTagsJob kaalChakraHandle LYT.Monthly jobInfo.jobData

createFetchUserDataJob ::
  ChakraJobs m r =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.Chakra ->
  LYT.KaalChakraJobData ->
  UTCTime ->
  m ()
createFetchUserDataJob merchantId merchantOperatingCityId chakra jobData scheduledTime = do
  now <- getCurrentTime
  jobs :: [AnyJob RiderJobType] <- QAllJ.getJobByTypeAndScheduleTime (show chakra) (addUTCTime 3600 now) (addUTCTime 3600 scheduledTime)
  when (length jobs == 0) $
    case chakra of
      LYT.Daily -> QAllJ.createJobByTime @_ @'Daily merchantId merchantOperatingCityId scheduledTime updJobData
      LYT.Weekly -> QAllJ.createJobByTime @_ @'Weekly merchantId merchantOperatingCityId scheduledTime updJobData
      LYT.Monthly -> QAllJ.createJobByTime @_ @'Monthly merchantId merchantOperatingCityId scheduledTime updJobData
      LYT.Quarterly -> QAllJ.createJobByTime @_ @'Quarterly merchantId merchantOperatingCityId scheduledTime updJobData
  where
    updJobData = jobData{startTime = Just scheduledTime}

createUpdateUserTagDataJob ::
  ChakraJobs m r =>
  Maybe (Id DM.Merchant) ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  LYT.Chakra ->
  LYT.UpdateKaalBasedTagsData ->
  UTCTime ->
  m ()
createUpdateUserTagDataJob merchantId merchantOperatingCityId chakra jobData scheduledTime = do
  now <- getCurrentTime
  jobs :: [AnyJob RiderJobType] <- QAllJ.getJobByTypeAndScheduleTime (show $ chkaraToJobUpdateType chakra) (addUTCTime 3600 now) (addUTCTime 3600 scheduledTime)
  when (length jobs == 0) $
    case chakra of
      LYT.Daily -> QAllJ.createJobByTime @_ @'DailyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
      LYT.Weekly -> QAllJ.createJobByTime @_ @'WeeklyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
      LYT.Monthly -> QAllJ.createJobByTime @_ @'MonthlyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
      LYT.Quarterly -> QAllJ.createJobByTime @_ @'QuarterlyUpdateTag merchantId merchantOperatingCityId scheduledTime jobData
  where
    chkaraToJobUpdateType ch = case ch of
      LYT.Daily -> DailyUpdateTag
      LYT.Weekly -> WeeklyUpdateTag
      LYT.Monthly -> MonthlyUpdateTag
      LYT.Quarterly -> QuarterlyUpdateTag
