module SharedLogic.Scheduler.Jobs.PassExpiryReminderMaster where

import qualified Data.Time as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified SharedLogic.PassExpiryReminder as SPER
import Tools.Error

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
runPassExpiryReminderMaster Job {merchantId, merchantOperatingCityId} = do
  merchantId' <- merchantId & fromMaybeM (InternalError "Job is missing merchantId")
  merchantOperatingCityId' <- merchantOperatingCityId & fromMaybeM (InternalError "Job is missing merchantOperatingCityId")
  SPER.sendPassExpiryReminderPns (merchantId' :: Id DM.Merchant) (merchantOperatingCityId' :: Id DMOC.MerchantOperatingCity)
  ReSchedule . Time.addUTCTime Time.nominalDay <$> getCurrentTime -- Reschedule for next day
