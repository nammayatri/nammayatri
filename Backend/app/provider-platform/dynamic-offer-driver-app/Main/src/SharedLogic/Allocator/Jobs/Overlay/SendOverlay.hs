{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.Allocator.Jobs.Overlay.SendOverlay where

import Control.Lens ((.~))
import Data.Generics.Labels ()
import Data.List (nub)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverInformation as DTDI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Overlay as DOverlay
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleCategory as DVC
import EulerHS.Prelude hiding (id, (^..), (.~))
import Kernel.External.Types
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.Allocator as SAllocator
import qualified SharedLogic.DriverFee as SLDriverFee
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Tools.Notifications as TN

acUsageRestrictionKey :: Text
acUsageRestrictionKey = "AC_USAGE_RESTRICTED"

acUsageRestrictionLiftedKey :: Text
acUsageRestrictionLiftedKey = "AC_USAGE_RESTRICTION_LIFTED"

acUsageWarningKey :: Text
acUsageWarningKey = "AC_USAGE_WARNING"

sendACUsageRestrictionLiftedOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
sendACUsageRestrictionLiftedOverlay = sendACOverlay acUsageRestrictionLiftedKey

sendACUsageRestrictionOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
sendACUsageRestrictionOverlay = sendACOverlay acUsageRestrictionKey

sendACUsageWarningOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> m ()
sendACUsageWarningOverlay = sendACOverlay acUsageWarningKey

sendACOverlay :: (CacheFlow m r, EsqDBFlow m r) => Text -> DP.Person -> m ()
sendACOverlay overlayKey person = do
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory person.merchantOperatingCityId overlayKey (fromMaybe ENGLISH person.language) Nothing Nothing Nothing
  whenJust mOverlay $ \overlay -> do
    TN.sendOverlay person.merchantOperatingCityId person $ TN.mkOverlayReq overlay

sendOverlayToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    ServiceFlow m r,
    Esq.Transactionable m,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'SAllocator.SendOverlay ->
  m ExecutionResult
sendOverlayToDriver (Job {id, jobInfo}) = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      serviceName = fromMaybe DPlan.YATRI_SUBSCRIPTION jobData.serviceName
      mbMerchantOperatingCityId = jobData.merchantOperatingCityId
      jobId = id
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOperatingCityId merchant Nothing
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY jobData.vehicleCategory
  driverIds <- nub <$> getBatchedDriverIds merchantId merchantOpCityId jobId jobData.condition jobData.freeTrialDays jobData.timeDiffFromUtc jobData.driverPaymentCycleDuration jobData.driverPaymentCycleStartTime jobData.overlayBatchSize serviceName vehicleCategory
  logInfo $ "The Job " <> jobId.getId <> " is scheduled for following driverIds " <> show driverIds
  let (conditionKey, isVehCatBased) = castConditionsToSchedulerTypeAndIsVehCatBased jobData.condition
  driverIdsLength <- getSendOverlaySchedulerDriverIdsLength merchantOpCityId (if isVehCatBased then Just vehicleCategory else Nothing) (Just conditionKey)
  if driverIdsLength > 0 || (invoiceOverlayCondition jobData.condition && not (null driverIds))
    then do
      mapM_ (sendOverlayAccordingToCondition jobData serviceName vehicleCategory) driverIds
      ReSchedule . addUTCTime 1 <$> getCurrentTime
    else do
      unless (null driverIds) $ mapM_ (sendOverlayAccordingToCondition jobData serviceName vehicleCategory) driverIds
      case jobData.rescheduleInterval of
        Just interval -> do
          lastScheduledTime <- getLastScheduledJobTime merchantOpCityId vehicleCategory jobId jobData.scheduledTime jobData.timeDiffFromUtc
          let newScheduledTime = addUTCTime (fromIntegral interval) lastScheduledTime
          setLastScheduledJobTime merchantOpCityId vehicleCategory jobId newScheduledTime
          pure (ReSchedule newScheduledTime)
        Nothing -> return Complete
  where
    invoiceOverlayCondition condition = condition == DOverlay.InvoiceGenerated DPlan.MANUAL || condition == DOverlay.InvoiceGenerated DPlan.AUTOPAY
    sendOverlayAccordingToCondition jobDataInfo serviceName vehicleCategory driverId = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      case jobDataInfo.condition of
        DOverlay.PaymentOverdueGreaterThan limit -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when (manualDues > fromIntegral limit) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues (Just vehicleCategory)
        DOverlay.PaymentOverdueBetween rLimit lLimit -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when ((fromIntegral rLimit <= manualDues) && (manualDues <= fromIntegral lLimit)) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues (Just vehicleCategory)
        DOverlay.InvoiceGenerated _ -> do
          manualDues <- getAllManualDuesWithoutFilter driverId serviceName
          sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues (Just vehicleCategory)
        DOverlay.FreeTrialDaysLeft _ -> do
          driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
          currUdf1 <- getCurrentAutoPayStatusUDF driverInfo
          when (currUdf1 == jobDataInfo.udf1) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 0 (Just vehicleCategory)
        DOverlay.BlockedDrivers -> do
          manualDues <- getManualDues driverId jobDataInfo.timeDiffFromUtc jobDataInfo.driverFeeOverlaySendingTimeLimitInDays serviceName
          when (manualDues > 0) $ sendOverlay driver jobDataInfo.overlayKey jobDataInfo.udf1 manualDues (Just vehicleCategory)
        _ -> pure ()

    getCurrentAutoPayStatusUDF driverInfo = do
      case driverInfo.autoPayStatus of
        Nothing -> return $ Just "PLAN_NOT_SELECTED"
        Just DTDI.ACTIVE -> return $ Just ""
        Just DTDI.PENDING -> return $ Just ""
        _ -> return $ Just "AUTOPAY_NOT_SET"

    castConditionsToSchedulerTypeAndIsVehCatBased :: DOverlay.OverlayCondition -> (Text, Bool)
    castConditionsToSchedulerTypeAndIsVehCatBased condition = case condition of
      DOverlay.PaymentOverdueGreaterThan _ -> ("PaymentOverdueGreaterThan", True)
      DOverlay.PaymentOverdueBetween _ _ -> ("PaymentOverdueBetween", True)
      DOverlay.BlockedDrivers -> ("BlockedDrivers", True)
      DOverlay.FreeTrialDaysLeft _ -> ("FreeTrialDaysLeft", False)
      _ -> ("InvoiceGenerated", False)

    getAllManualDuesWithoutFilter driverId serviceName = do
      pendingDriverFees <- QDF.findAllFeeByTypeServiceStatusAndDriver serviceName driverId [DDF.RECURRING_INVOICE] [DDF.PAYMENT_OVERDUE]
      return $ sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

    getManualDues driverId timeDiffFromUtc driverFeeOverlaySendingTimeLimitInDays serviceName = do
      windowEndTime <- getLocalCurrentTime timeDiffFromUtc
      let windowStartTime = addUTCTime (-1 * fromIntegral driverFeeOverlaySendingTimeLimitInDays * 86400) (UTCTime (utctDay windowEndTime) (secondsToDiffTime 0))
      pendingDriverFees <- QDF.findAllFeeByTypeServiceStatusAndDriver serviceName driverId [DDF.RECURRING_INVOICE] [DDF.PAYMENT_OVERDUE]
      let filteredDriverFees = filter (\driverFee -> driverFee.startTime >= windowStartTime) pendingDriverFees
      return $
        if null filteredDriverFees
          then 0
          else sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) pendingDriverFees

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

sendOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Text -> Maybe Text -> HighPrecMoney -> Maybe DVC.VehicleCategory -> m ()
sendOverlay driver overlayKey udf1 amount mbVehicle = do
  mOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory driver.merchantOperatingCityId overlayKey (fromMaybe ENGLISH driver.language) udf1 mbVehicle Nothing
  whenJust mOverlay $ \overlay -> do
    let okButtonText = T.replace (templateText "dueAmount") (show amount) <$> overlay.okButtonText
    let description = T.replace (templateText "dueAmount") (show amount) <$> overlay.description
    let overlay' :: DOverlay.Overlay = overlay & #okButtonText .~ okButtonText & #description .~ description
    fork ("sending overlay to driver with driverId " <> (show driver.id)) $ do
      TN.sendOverlay driver.merchantOperatingCityId driver $ TN.mkOverlayReq overlay'

getSendOverlaySchedulerDriverIdsLength :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DVC.VehicleCategory -> Maybe Text -> m Integer
getSendOverlaySchedulerDriverIdsLength merchantOpCityId vehicleCategory mbOperationName = Hedis.lLen $ makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName

getFirstNSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DVC.VehicleCategory -> Maybe Text -> Integer -> m [Id DP.Person]
getFirstNSendOverlaySchedulerDriverIds merchantOpCityId vehicleCategory mbOperationName num = Hedis.lRange (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName) 0 (num -1)

deleteNSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DVC.VehicleCategory -> Maybe Text -> Integer -> m ()
deleteNSendOverlaySchedulerDriverIds merchantOpCityId vehicleCategory mbOperationName num = Hedis.lTrim (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName) num (-1)

addSendOverlaySchedulerDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe DVC.VehicleCategory -> Maybe Text -> NonEmpty (Id DP.Person) -> m ()
addSendOverlaySchedulerDriverIds merchantOpCityId vehicleCategory mbOperationName nePIds = do
  Hedis.rPush (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName) nePIds
  Hedis.expire (makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName) (20 * 3600)

makeSendOverlaySchedulerDriverIdsKey :: Id DMOC.MerchantOperatingCity -> Maybe DVC.VehicleCategory -> Maybe Text -> Text
makeSendOverlaySchedulerDriverIdsKey merchantOpCityId vehicleCategory mbOperationName =
  do
    "SendOverlayScheduler:merchantOpCityId-"
    <> merchantOpCityId.getId
    <> maybe "" ((":VehCat:-" <>) . show) vehicleCategory
    <> maybe "" (":Operation:-" <>) mbOperationName

freeTrialDaysSetKey :: Text
freeTrialDaysSetKey = "SendOverlayScheduler:FreeTrialDaysSet:"

getLastScheduledJobTime :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> DVC.VehicleCategory -> Id AnyJob -> TimeOfDay -> Seconds -> m UTCTime
getLastScheduledJobTime merchantOpCityId vehicleCategory jobId scheduledTime timeDiffFromUtc = do
  Hedis.withMasterRedis $
    Hedis.get (makeLastScheduledTimeJobKey merchantOpCityId vehicleCategory jobId) >>= \case
      Nothing -> do
        now <- getLocalCurrentTime timeDiffFromUtc
        let lastScheduledTime = addUTCTime (fromIntegral $ -1 * timeDiffFromUtc) (UTCTime (utctDay now) (timeOfDayToTime scheduledTime))
        setLastScheduledJobTime merchantOpCityId vehicleCategory jobId lastScheduledTime
        pure lastScheduledTime
      Just lastScheduledTime -> pure lastScheduledTime

setLastScheduledJobTime :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> DVC.VehicleCategory -> Id AnyJob -> UTCTime -> m ()
setLastScheduledJobTime merchantOpCityId vehicleCategory jobId = Hedis.set (makeLastScheduledTimeJobKey merchantOpCityId vehicleCategory jobId)

makeLastScheduledTimeJobKey :: Id DMOC.MerchantOperatingCity -> DVC.VehicleCategory -> Id AnyJob -> Text
makeLastScheduledTimeJobKey merchantOpCityId vehicleCategory jobId = "SendOverlayScheduler:lastScheduledTime:merchantOpCityId-" <> merchantOpCityId.getId <> ":VehCat:-" <> show vehicleCategory <> ":jobId" <> jobId.getId

getBatchedDriverIds ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id AnyJob ->
  DOverlay.OverlayCondition ->
  Int ->
  Seconds ->
  NominalDiffTime ->
  NominalDiffTime ->
  Int ->
  DPlan.ServiceNames ->
  DVC.VehicleCategory ->
  m [Id DP.Person]
getBatchedDriverIds merchantId merchantOperatingCityId _ condition freeTrialDays timeDiffFromUtc driverPaymentCycleDuration driverPaymentCycleStartTime overlayBatchSize serviceName vehicleCategory = do
  case condition of
    DOverlay.InvoiceGenerated paymentMode -> do
      now <- getLocalCurrentTime timeDiffFromUtc
      let potentialStartToday = addUTCTime driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
          startTime = addUTCTime (-1 * driverPaymentCycleDuration) potentialStartToday
          endTime = addUTCTime 120 potentialStartToday
      driverFees <- QDF.findWindowsAndServiceNameWithFeeTypeAndLimitAndServiceName merchantId merchantOperatingCityId startTime endTime (getFeeType paymentMode) overlayBatchSize serviceName
      let driverIds = driverFees <&> (.driverId)
      void $ QDF.updateDriverFeeOverlayScheduledByServiceName driverIds True startTime endTime serviceName
      return driverIds
    DOverlay.PaymentOverdueGreaterThan _ -> getDriverIdsBatched (Just "PaymentOverdueGreaterThan") (Just vehicleCategory)
    DOverlay.PaymentOverdueBetween _ _ -> getDriverIdsBatched (Just "PaymentOverdueBetween") (Just vehicleCategory)
    DOverlay.BlockedDrivers -> getDriverIdsBatched (Just "BlockedDrivers") (Just vehicleCategory)
    DOverlay.FreeTrialDaysLeft numOfDays -> do
      driverIdsLength <- getSendOverlaySchedulerDriverIdsLength merchantOperatingCityId Nothing (Just "FreeTrialDaysLeft")
      when (driverIdsLength < 1) do
        driverIds <- do
          now <- getCurrentTime
          let startTime = addUTCTime (-1 * fromIntegral timeDiffFromUtc) $ addUTCTime (-1 * 86400 * fromIntegral (freeTrialDays - (numOfDays - 1))) (UTCTime (utctDay now) (secondsToDiffTime 0))
              endTime = addUTCTime 86400 startTime
          QDI.findAllByEnabledAtInWindow merchantOperatingCityId (Just startTime) (Just endTime) <&> (<&> (.driverId))
        whenJust (nonEmpty driverIds) $ addSendOverlaySchedulerDriverIds merchantOperatingCityId Nothing (Just "FreeTrialDaysLeft")
      getDriverIdsBatched (Just "FreeTrialDaysLeft") Nothing
    _ -> return []
  where
    getFeeType paymentMode = case paymentMode of
      DPlan.AUTOPAY -> DDF.RECURRING_EXECUTION_INVOICE
      DPlan.MANUAL -> DDF.RECURRING_INVOICE
    getDriverIdsBatched mbOperation mbVehicleCategory = do
      batchedDriverIds <- getFirstNSendOverlaySchedulerDriverIds merchantOperatingCityId mbVehicleCategory mbOperation $ fromIntegral overlayBatchSize
      deleteNSendOverlaySchedulerDriverIds merchantOperatingCityId mbVehicleCategory mbOperation $ fromIntegral overlayBatchSize
      return batchedDriverIds

templateText :: Text -> Text
templateText txt = "{#" <> txt <> "#}"
