module SharedLogic.Allocator.Jobs.Mandate.RetryAutopayCollection (retryAutopayCollection) where

import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Person as P
import qualified Domain.Types.Plan as Plan
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id (cast)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.Payment (mkInvoiceAgainstDriverFee)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverPlan as QDP
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Notification as QNTF

retryLogTag :: Text
retryLogTag = "[RETRY_AUTOPAY_COLLECTION]"

defaultRetryBatchSize :: Int
defaultRetryBatchSize = 500

notificationJobScheduleDelay :: NominalDiffTime
notificationJobScheduleDelay = 300

retryWindowLockTtl :: Redis.ExpirationTime
retryWindowLockTtl = 900

retryWindowCloseOutTtl :: Redis.ExpirationTime
retryWindowCloseOutTtl = 3600 * 12

retryWindowKey :: Text -> Text -> Text -> Text -> UTCTime -> UTCTime -> Text
retryWindowKey purpose merchantId merchantOpCityId serviceName startTime endTime =
  "RetryAutopayCollection:" <> purpose <> ":" <> merchantId <> ":" <> merchantOpCityId <> ":" <> serviceName <> ":" <> show startTime <> ":" <> show endTime

retryAutopayCollection ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'RetryAutopayCollection ->
  m ExecutionResult
retryAutopayCollection Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      startTime = jobData.startTime
      endTime = jobData.endTime
      serviceName = fromMaybe Plan.YATRI_SUBSCRIPTION jobData.serviceName
      convertedSoFar = fromMaybe 0 jobData.convertedSoFar
      eligibleStages = [DF.EXECUTION_SCHEDULED, DF.NOTIFICATION_SCHEDULED] <> [DF.NOTIFICATION_ATTEMPTING | jobData.includeNotificationAttempting]
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId jobData.merchantOperatingCityId merchant Nothing
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let batchSize = fromMaybe defaultRetryBatchSize jobData.batchSize
      remainingToConvert = maybe batchSize (\maxFees -> maxFees - convertedSoFar) jobData.maxFeesToConvert
      windowKey purpose = retryWindowKey purpose merchantId.getId merchantOpCityId.getId (show serviceName) startTime endTime
      closeOutWindow = do
        logInfo $ retryLogTag <> " done for window, " <> show convertedSoFar <> " driver fees converted, dryRun " <> show jobData.dryRun
        when (convertedSoFar > 0 && not jobData.dryRun) $ do
          isFirstCloseOut <- Redis.setNxExpire (windowKey "CloseOut") retryWindowCloseOutTtl True
          when isFirstCloseOut $
            Redis.runInMasterCloudRedisCell $
              createJobIn @_ @'SendPDNNotificationToDriver (Just merchantId) (Just merchantOpCityId) notificationJobScheduleDelay $
                SendPDNNotificationToDriverJobData
                  { merchantId = merchantId,
                    merchantOperatingCityId = Just merchantOpCityId,
                    startTime = startTime,
                    endTime = endTime,
                    retryCount = Just 0,
                    serviceName = Just serviceName,
                    shardNum = Nothing
                  }
        return Complete
      processBatch = do
        driverFees <- QDF.findDriverFeeInRangeEligibleForAutopayRetry merchantId merchantOpCityId batchSize jobData.lastDriverFeeId startTime endTime serviceName eligibleStages
        if null driverFees
          then closeOutWindow
          else do
            eligibleDriverFees <- take remainingToConvert <$> filterEligibleForRetry serviceName driverFees
            convertedDriverFees <- if jobData.dryRun then return [] else convertToAutoPay eligibleStages eligibleDriverFees
            let newLastDriverFeeId = (.id.getId) <$> listToMaybe (reverse driverFees)
                newConvertedSoFar = convertedSoFar + (if jobData.dryRun then length eligibleDriverFees else length convertedDriverFees)
            Redis.runInMasterCloudRedisCell $
              createJobIn @_ @'RetryAutopayCollection (Just merchantId) (Just merchantOpCityId) transporterConfig.mandateNotificationRescheduleInterval $
                RetryAutopayCollectionJobData
                  { merchantId = merchantId,
                    merchantOperatingCityId = Just merchantOpCityId,
                    startTime = startTime,
                    endTime = endTime,
                    serviceName = Just serviceName,
                    batchSize = Just batchSize,
                    lastDriverFeeId = newLastDriverFeeId,
                    dryRun = jobData.dryRun,
                    maxFeesToConvert = jobData.maxFeesToConvert,
                    convertedSoFar = Just newConvertedSoFar,
                    includeNotificationAttempting = jobData.includeNotificationAttempting
                  }
            return Complete
  if remainingToConvert <= 0
    then closeOutWindow
    else do
      batchResult <- Redis.whenWithLockRedisAndReturnValue (windowKey "Lock") retryWindowLockTtl processBatch
      case batchResult of
        Right result -> return result
        Left () -> do
          logInfo $ retryLogTag <> " window locked by another run, rescheduling this batch"
          ReSchedule . addUTCTime transporterConfig.mandateNotificationRescheduleInterval <$> getCurrentTime

filterEligibleForRetry ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Plan.ServiceNames ->
  [DriverFee] ->
  m [DriverFee]
filterEligibleForRetry serviceName driverFees = do
  driverPlans <- QDP.findAllByDriverIdsPaymentModeAndServiceName (driverFees <&> (.driverId)) Plan.AUTOPAY serviceName (Just DI.ACTIVE)
  activeInvoices <- QINV.findAllActiveByDriverFeeIds (driverFees <&> (.id))
  let mandateByDriverId = Map.fromList $ mapMaybe (\driverPlan -> (\mandateId -> (driverPlan.driverId, mandateId)) <$> driverPlan.mandateId) driverPlans
      driverFeeIdsWithLiveManualInvoice = filter ((/= INV.AUTOPAY_INVOICE) . (.paymentMode)) activeInvoices <&> (.driverFeeId)
      driverFeesWithActiveMandate =
        filter
          ( \driverFee ->
              Map.member (cast @P.Driver @P.Person driverFee.driverId) mandateByDriverId
                && driverFee.id `notElem` driverFeeIdsWithLiveManualInvoice
          )
          driverFees
  filterM (fmap not . isManualPaymentInProgress) driverFeesWithActiveMandate

isManualPaymentInProgress :: (CacheFlow m r, MonadFlow m) => DriverFee -> m Bool
isManualPaymentInProgress driverFee = do
  mbInProgress <- Redis.runInMasterCloudRedisCell $ Redis.get (manualPaymentInProgressKey driverFee.id.getId)
  return $ mbInProgress == Just True

convertToAutoPay ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  [AutopayPaymentStage] ->
  [DriverFee] ->
  m [DriverFee]
convertToAutoPay eligibleStages driverFees = do
  let driverFeeIds = driverFees <&> (.id)
  QNTF.updateSuccessToFailedByDriverFeeIds driverFeeIds
  QINV.updateActiveInvoiceStatusByDriverFeeIdsAndPaymentMode INV.INACTIVE driverFeeIds INV.AUTOPAY_INVOICE
  forM driverFees convertDriverFee
  where
    convertDriverFee driverFee = do
      now <- getCurrentTime
      let reconvertedDriverFee =
            driverFee
              { DF.feeType = DF.RECURRING_EXECUTION_INVOICE,
                DF.status = DF.PAYMENT_PENDING,
                DF.autopayPaymentStage = Just DF.NOTIFICATION_SCHEDULED,
                DF.stageUpdatedAt = Just now,
                DF.notificationRetryCount = 0,
                DF.updatedAt = now
              }
      invoiceId <- generateGUID
      invoiceShortId <- generateShortId
      QINV.create $ mkInvoiceAgainstDriverFee (invoiceId :: Text) invoiceShortId.getShortId now Nothing INV.AUTOPAY_INVOICE reconvertedDriverFee
      QDF.updateManualToAutoPayForRetry eligibleStages driverFee.id
      return reconvertedDriverFee
