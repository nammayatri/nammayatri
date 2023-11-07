module SharedLogic.Allocator.Jobs.DriverFeeUpdates.BadDebtCalculationScheduler where

import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.DriverFee as QDF

badDebtCalculation ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  Job 'BadDebtCalculation ->
  m ExecutionResult
badDebtCalculation Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
  transporterConfig <- SCT.findByMerchantOpCityId opCityId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFeesToUpdate <- QDF.findAllDriverFeesRequiredToMovedIntoBadDebt merchantId transporterConfig
  void $ QDF.updateBadDebtDateAllDriverFeeIds merchantId (driverFeesToUpdate <&> (.id)) transporterConfig
  if null driverFeesToUpdate
    then return Complete
    else ReSchedule <$> getRescheduledTime transporterConfig

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.badDebtRescheduleTime <$> getCurrentTime
