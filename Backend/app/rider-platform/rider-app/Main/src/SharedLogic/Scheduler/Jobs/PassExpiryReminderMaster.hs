module SharedLogic.Scheduler.Jobs.PassExpiryReminderMaster where

import qualified Data.Time as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderConfig as DRC
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import qualified SharedLogic.PassExpiryReminder as SPER
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Tools.Error

batchRescheduleDelaySeconds :: NominalDiffTime
batchRescheduleDelaySeconds = 2

runPassExpiryReminderMaster ::
  ( ServiceFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'PassExpiryReminderMaster ->
  m ExecutionResult
runPassExpiryReminderMaster Job {id, merchantId, merchantOperatingCityId, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  merchantId' <- merchantId & fromMaybeM (InternalError "Job is missing merchantId")
  merchantOpCityId <- merchantOperatingCityId & fromMaybeM (InternalError "Job is missing merchantOperatingCityId")
  let cursor = jobInfo.jobData.cursor
  mbNextCursor <- SPER.sendPassExpiryReminderBatch merchantId' merchantOpCityId cursor
  case mbNextCursor of
    Just nextCursor -> do
      createJobIn @_ @'PassExpiryReminderMaster (Just merchantId') (Just merchantOpCityId) batchRescheduleDelaySeconds (PassExpiryReminderMasterJobData {cursor = Just nextCursor})
      pure Complete
    Nothing -> do
      scheduleTomorrow merchantId' merchantOpCityId
      pure Complete

scheduleTomorrow ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
scheduleTomorrow merchantId' merchantOpCityId = do
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) (mbRiderConfig :: Maybe DRC.RiderConfig)
      tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
  now <- getCurrentTime
  let nextRunLocalTime = fromMaybe (Time.TimeOfDay 1 0 0) (mbRiderConfig >>= (.passExpiryReminderNextRunLocalTime))
      localNow = Time.addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
      tomorrow = Time.addDays 1 (Time.utctDay localNow)
      localTimeNextRun = Time.LocalTime tomorrow nextRunLocalTime
      tomorrowAtTargetUTC = Time.localTimeToUTC tz localTimeNextRun
      delay = diffUTCTime tomorrowAtTargetUTC now
      lockKey = "PassExpiryReminderMaster:nextRun:" <> merchantOpCityId.getId <> ":" <> show tomorrow
      lockTtlSeconds = max 60 (round delay + 1800)
      nextJobData = PassExpiryReminderMasterJobData {cursor = Nothing}
  claimed <- Hedis.tryLockRedis lockKey lockTtlSeconds
  if claimed
    then do
      createJobIn @_ @'PassExpiryReminderMaster (Just merchantId') (Just merchantOpCityId) delay nextJobData
      logInfo $ "PassExpiryReminderMaster: drained, scheduled next run at tomorrow " <> show nextRunLocalTime <> " local"
    else logInfo "PassExpiryReminderMaster: drained, next-day run already enqueued — skipping reschedule"
