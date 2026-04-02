module SharedLogic.Allocator.Jobs.Insurance.IffcoTokioInsurance
  ( triggerIffcoTokioInsuranceForOnRideDrivers,
  )
where

import qualified Domain.Types.DriverInformation as DI
import Domain.Types.Trip (isInterCityTrip, isRentalTrip)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.IffcoTokioInsurance as IffcoInsurance
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.DriverInformationExtra as QDIE

triggerIffcoTokioInsuranceForOnRideDrivers ::
  ( EncFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    Log m
  ) =>
  Job 'IffcoTokioInsurance ->
  m ExecutionResult
triggerIffcoTokioInsuranceForOnRideDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      batchSize = jobData.batchSize
      currentOffset = jobData.offset
      autoSchedule = jobData.autoSchedule

  drivers <- QDIE.findOnRideDriversWithRideStartedByMerchantOpCityIds [merchantOpCityId] batchSize currentOffset
  logInfo $ "IffcoTokioInsuranceJob: Processing batch offset=" <> show currentOffset <> " count=" <> show (length drivers)
  mapM_ processDriverInsurance drivers

  if autoSchedule && length drivers >= batchSize
    then do
      let nextOffset = currentOffset + batchSize
      createJobIn @_ @'IffcoTokioInsurance Nothing Nothing 0 $
        IffcoTokioInsuranceJobData
          { merchantOperatingCityId = merchantOpCityId,
            batchSize = batchSize,
            offset = nextOffset,
            autoSchedule = autoSchedule
          }
      pure Complete
    else do
      logInfo "IffcoTokioInsuranceJob: All batches processed."
      pure Complete

processDriverInsurance ::
  ( EncFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Log m
  ) =>
  DI.DriverInformation ->
  m ()
processDriverInsurance driverInfo = do
  let driverId = driverInfo.driverId
      mbMerchantId = driverInfo.merchantId
      mbMerchantOpCityId = driverInfo.merchantOperatingCityId
      mbTripCategory = driverInfo.onRideTripCategory
  case (mbMerchantId, mbMerchantOpCityId) of
    (Just merchantId, Just merchantOpCityId) -> do
      let shouldSkip = maybe False (\tc -> isInterCityTrip tc || isRentalTrip tc) mbTripCategory
      if shouldSkip
        then logTagInfo "IffcoTokioInsuranceJob" $
          "Skipping driver=" <> driverId.getId
            <> " tripCategory="
            <> show mbTripCategory
        else
          fork "IffcoTokio driver insurance" $
            IffcoInsurance.triggerIffcoTokioInsurance driverId merchantId merchantOpCityId
    _ ->
      logTagInfo "IffcoTokioInsuranceJob" $
        "Skipping driver=" <> driverInfo.driverId.getId <> " missing merchantId or merchantOperatingCityId"
