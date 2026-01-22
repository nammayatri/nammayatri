module SharedLogic.Allocator.Jobs.DriverFeeUpdates.BadDebtCalculationScheduler where

import qualified Data.Map as M
import Data.Time (UTCTime (UTCTime, utctDay), fromGregorian, secondsToDiffTime, toGregorian)
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.Queries.DriverFee as QDF

badDebtCalculation ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'BadDebtCalculation ->
  m ExecutionResult
badDebtCalculation Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
  transporterConfig <- SCTC.findByMerchantOpCityId opCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFeesToUpdate <- B.runInReplica $ QDF.findAllDriverFeesRequiredToMovedIntoBadDebt merchantId transporterConfig
  void $ QDF.updateBadDebtDateAllDriverFeeIds merchantId (driverFeesToUpdate <&> (.id)) transporterConfig
  if null driverFeesToUpdate
    then do
      scheduledTime <- getThirdDayOfNextMonthWithMidnight
      now <- getCurrentTime
      let diffTime = diffUTCTime scheduledTime now
      let dfCalculationJobTs = diffTime + transporterConfig.badDebtSchedulerTime
      createJobIn @_ @'BadDebtCalculation (Just merchantId) (Just opCityId) dfCalculationJobTs $
        BadDebtCalculationJobData
          { merchantId = merchantId,
            merchantOperatingCityId = opCityId
          }
      return Complete
    else ReSchedule <$> getRescheduledTime transporterConfig

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.badDebtRescheduleTime <$> getCurrentTime

getThirdDayOfNextMonthWithMidnight :: (MonadTime m) => m UTCTime
getThirdDayOfNextMonthWithMidnight = do
  currentDay <- getCurrentTime >>= pure . utctDay
  let (year, month, _) = toGregorian currentDay
      nextMonth = if month == 12 then 1 else month + 1
      nextYear = if month == 12 then year + 1 else year
      thirdDayOfNextMonth = fromGregorian nextYear nextMonth 3
      midnightTime = UTCTime thirdDayOfNextMonth (secondsToDiffTime 0)
  return midnightTime
