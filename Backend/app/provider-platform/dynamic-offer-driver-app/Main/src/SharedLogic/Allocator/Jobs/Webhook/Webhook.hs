module SharedLogic.Allocator.Jobs.Webhook.Webhook where

import qualified Data.Map as M
import qualified Domain.Action.WebhookHandler as AWebhook
import Kernel.External.Encryption (getHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.DriverFee (jobDuplicationPreventionKey)
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Webhook ()

sendWebhookWithRetryToExternal ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Job 'SendWebhookToExternal ->
  m ExecutionResult
sendWebhookWithRetryToExternal Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  ----- added measure duration for debugging -------
  (response, timetaken) <- measureDuration $ do
    let jobData = jobInfo.jobData
        webhookData :: AWebhook.WebhookJobInfo = jobData.webhookData
    response <- AWebhook.sendWebhookWithRetry webhookData
    case response of
      AWebhook.PROCESS_NEXT_BATCH rescheduleInterval -> ReSchedule <$> getRescheduledTime (secondsToNominalDiffTime $ Seconds rescheduleInterval)
      AWebhook.SCHEDULE_NEXT_JOB scheduledAtGap retryCount' -> do
        let newJobWebhookData :: AWebhook.WebhookJobInfo = mkJoBInfo webhookData (Just retryCount')
            newJobData =
              SendWebhookToExternalJobData
                { webhookData = newJobWebhookData
                }
        let jobDataT :: Text = show newJobData
        hashedJobData <- getHash jobDataT
        duplicationKey <- Hedis.setNxExpire (jobDuplicationPreventionKey hashedJobData "Webhook_Fan_out") (3600 * 12) True
        when duplicationKey do
          createJobIn @_ @'SendWebhookToExternal Nothing Nothing (secondsToNominalDiffTime $ Seconds scheduledAtGap) $
            SendWebhookToExternalJobData
              { webhookData = newJobWebhookData
              }
        return Complete
      _ -> return Complete
  logWarning ("duration of job " <> show timetaken)
  return response
  where
    mkJoBInfo AWebhook.WebhookJobInfo {..} retryCount' =
      AWebhook.WebhookJobInfo
        { retryCount = retryCount',
          ..
        }

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime
