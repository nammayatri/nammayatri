module SharedLogic.Allocator.Jobs.Cautio.InstallationStatus where

import qualified Data.Map as M
import qualified Domain.Types.Extra.MerchantServiceConfig as DMSC
import qualified Domain.Types.Plan as Plan
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import qualified Kernel.Types.Id as KID
import Kernel.Utils.Common
import Lib.Dashcam.Domain.Cautio.Types as Cautio
import qualified Lib.Dashcam.Domain.Types as Dashcam
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.Queries.DriverPlan
import qualified Tools.Dashcam as TD

installationStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasKafkaProducer r
  ) =>
  Job 'CheckDashCamInstallationStatus ->
  m ExecutionResult
installationStatus Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  ----- added measure duration for debugging -------
  (response, timetaken) <- measureDuration $ do
    let jobData = jobInfo.jobData
        merchantId = jobData.merchantId
        merchantOpCityId = jobData.merchantOperatingCityId
        serviceName = Plan.DASHCAM_RENTAL Plan.CAUTIO
    offset <- getOffsetCount $ mkOffsetKey merchantOpCityId.getId
    subscriptionConfigs <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing serviceName >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName)
    let batchSize = subscriptionConfigs.genericBatchSizeForJobs
        rescheduleInterval = subscriptionConfigs.genericJobRescheduleTime
    resp <- withTryCatch "cautioInstallationStatus:installationStatus" $ TD.cautioInstallationStatus merchantId merchantOpCityId (DMSC.DashCamService Dashcam.Cautio) (mkCautioReq offset batchSize)
    dataEntity <- do
      case resp of
        Left err -> do
          logError ("error while calling installation api :-" <> show err)
          return []
        Right response -> return response
    case dataEntity of
      [] -> do
        createJobIn @_ @'CheckDashCamInstallationStatus (Just merchantId) (Just merchantOpCityId) subscriptionConfigs.genericNextJobScheduleTimeThreshold $ do
          CheckDashCamInstallationStatusJobData
            { merchantId = merchantId,
              merchantOperatingCityId = merchantOpCityId
            }
        return Complete
      dataArray -> do
        forM_ dataArray $ \dataPoint -> do
          let driverId = dataPoint.cautioDriverId
              enableSeviceCharge = (enableServiceCharge dataPoint.status) && dataPoint.installed
          updateEnableServiceUsageChargeByDriverIdAndServiceName enableSeviceCharge (KID.Id driverId) serviceName
        incrOffsetKey $ mkOffsetKey merchantOpCityId.getId
        ReSchedule <$> getRescheduledTime rescheduleInterval
  logWarning ("duration of job " <> show timetaken)
  return response
  where
    mkCautioReq offset batchSize = InstallationStatusReq $ mkCautioReqParams offset batchSize
    mkCautioReqParams offset batchSize = ParamsCautio mkPathCautio (mkQueryStringCautio offset batchSize)
    mkPathCautio = PathCautio {organisation_id = ""}
    mkQueryStringCautio offset batchSize = QueryStringCautio {page = offset, pageSize = batchSize}
    enableServiceCharge status = do
      case status of
        Cautio.NOT_INSTALLED -> False
        Cautio.INSTALLED -> True

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

mkOffsetKey :: Text -> Text
mkOffsetKey opCityId = "Cautio:MocId:" <> opCityId <> ":Offset:-"

getOffsetCount :: (MonadFlow m, CacheFlow m r) => Text -> m Int
getOffsetCount key = do
  count <- do
    Hedis.get key >>= \case
      Just count -> return count
      Nothing -> return 0
  return count

incrOffsetKey :: (MonadFlow m, CacheFlow m r) => Text -> m ()
incrOffsetKey key = do
  void $ Hedis.incr key
  Hedis.expire key (60 * 60 * 6)
