module SharedLogic.Allocator.Jobs.DriverFeeUpdates.BadDebtCalculationScheduler where

import qualified Data.Map as M
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.DriverFee as QDF

badDebtCalculation ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Job 'BadDebtCalculation ->
  m ExecutionResult
badDebtCalculation Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
  transporterConfig <- SCT.findByMerchantOpCityId opCityId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFeesToUpdate <- B.runInReplica $ QDF.findAllDriverFeesRequiredToMovedIntoBadDebt merchantId transporterConfig
  void $ QDF.updateBadDebtDateAllDriverFeeIds merchantId (driverFeesToUpdate <&> (.id)) transporterConfig
  if null driverFeesToUpdate
    then do
      let dfCalculationJobTs = transporterConfig.badDebtSchedulerTime
      maxShards <- asks (.maxShards)
      createJobIn @_ @'BadDebtCalculation dfCalculationJobTs maxShards $
        BadDebtCalculationJobData
          { merchantId = merchantId,
            merchantOperatingCityId = opCityId
          }
      return Complete
    else ReSchedule <$> getRescheduledTime transporterConfig

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.badDebtRescheduleTime <$> getCurrentTime
