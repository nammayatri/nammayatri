{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RiderConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderConfig.RiderConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RiderConfig.RiderConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findExotelAppletMappingByMOCID ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findExotelAppletMappingByMOCID merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderConfig.RiderConfig -> m ())
updateByPrimaryKey (Domain.Types.RiderConfig.RiderConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appUrl appUrl,
      Se.Set Beam.autoSendBookingDetailsViaWhatsapp autoSendBookingDetailsViaWhatsapp,
      Se.Set Beam.autoUnblockSafetyCenterAfterDays autoUnblockSafetyCenterAfterDays,
      Se.Set Beam.avgSpeedInKmPerHr (Just avgSpeedInKmPerHr),
      Se.Set Beam.bookingSyncStatusCallSecondsDiffThreshold bookingSyncStatusCallSecondsDiffThreshold,
      Se.Set Beam.cancellationPaymentDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) cancellationPaymentDelay),
      Se.Set Beam.collectAutoCompleteData collectAutoCompleteData,
      Se.Set Beam.collectMMIRouteData collectMMIRouteData,
      Se.Set Beam.csAlertTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) csAlertTriggerDelay),
      Se.Set Beam.cxAgentDetails cxAgentDetails,
      Se.Set Beam.distanceWeightage distanceWeightage,
      Se.Set Beam.driverReferredSearchReqExpiry driverReferredSearchReqExpiry,
      Se.Set Beam.dynamicLogicUpdatePassword (Just dynamicLogicUpdatePassword),
      Se.Set Beam.emailOtpConfig emailOtpConfig,
      Se.Set Beam.enableEmergencyContactAddedMessage enableEmergencyContactAddedMessage,
      Se.Set Beam.enableLocalPoliceSupport enableLocalPoliceSupport,
      Se.Set Beam.enableSupportForSafety enableSupportForSafety,
      Se.Set Beam.executePaymentDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) executePaymentDelay),
      Se.Set Beam.exotelAppIdMapping exotelAppIdMapping,
      Se.Set Beam.exotelStatusCheckSchedulerDelay (Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay),
      Se.Set Beam.feedbackAlertRatingThreshold (Just feedbackAlertRatingThreshold),
      Se.Set Beam.hardLimitForSafetyJobs (Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs),
      Se.Set Beam.incidentReportSupport (Just incidentReportSupport),
      Se.Set Beam.isAvoidToll isAvoidToll,
      Se.Set Beam.ivrTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay),
      Se.Set Beam.kaptureConfig kaptureConfig,
      Se.Set Beam.kaptureQueue kaptureQueue,
      Se.Set Beam.localPoliceNumber localPoliceNumber,
      Se.Set Beam.makeMultiModalSearch (Just makeMultiModalSearch),
      Se.Set Beam.maximumWalkDistance (Just maximumWalkDistance),
      Se.Set Beam.metroBookingAllowed metroBookingAllowed,
      Se.Set Beam.minRidesToBlock minRidesToBlock,
      Se.Set Beam.minRidesToShowCancellationRate minRidesToShowCancellationRate,
      Se.Set Beam.payoutBatchDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) payoutBatchDelay),
      Se.Set Beam.payoutBatchSize payoutBatchSize,
      Se.Set Beam.payoutReferralProgram (Just payoutReferralProgram),
      Se.Set Beam.payoutReferralStartDate (Just payoutReferralStartDate),
      Se.Set Beam.placeNameCacheExpiryDays placeNameCacheExpiryDays,
      Se.Set Beam.policeTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay),
      Se.Set Beam.postRideSafetyNotificationDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) postRideSafetyNotificationDelay),
      Se.Set Beam.safetyCheckEndTime safetyCheckEndTime,
      Se.Set Beam.safetyCheckStartTime safetyCheckStartTime,
      Se.Set Beam.sensitiveWords sensitiveWords,
      Se.Set Beam.sensitiveWordsForExactMatch sensitiveWordsForExactMatch,
      Se.Set Beam.settleCancellationFeeBeforeNextRide settleCancellationFeeBeforeNextRide,
      Se.Set Beam.specialZoneRadius specialZoneRadius,
      Se.Set Beam.thresholdCancellationPercentageToBlock thresholdCancellationPercentageToBlock,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.trackingShortUrlPattern trackingShortUrlPattern,
      Se.Set Beam.useUserSettingsForSafetyIVR (Just useUserSettingsForSafetyIVR),
      Se.Set Beam.videoFileSizeUpperLimit videoFileSizeUpperLimit,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  fromTType' (Beam.RiderConfigT {..}) = do
    now <- Kernel.Types.Common.getCurrentTime
    let payoutReferralStartDate_ = fromMaybe now payoutReferralStartDate
    pure $
      Just
        Domain.Types.RiderConfig.RiderConfig
          { appUrl = appUrl,
            autoSendBookingDetailsViaWhatsapp = autoSendBookingDetailsViaWhatsapp,
            autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
            avgSpeedInKmPerHr = fromMaybe 20 avgSpeedInKmPerHr,
            bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
            cancellationPaymentDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> cancellationPaymentDelay),
            collectAutoCompleteData = collectAutoCompleteData,
            collectMMIRouteData = collectMMIRouteData,
            csAlertTriggerDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> csAlertTriggerDelay),
            cxAgentDetails = cxAgentDetails,
            distanceWeightage = distanceWeightage,
            driverReferredSearchReqExpiry = driverReferredSearchReqExpiry,
            dynamicLogicUpdatePassword = fromMaybe "dummy-password" dynamicLogicUpdatePassword,
            emailOtpConfig = emailOtpConfig,
            enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
            enableLocalPoliceSupport = enableLocalPoliceSupport,
            enableSupportForSafety = enableSupportForSafety,
            executePaymentDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> executePaymentDelay),
            exotelAppIdMapping = exotelAppIdMapping,
            exotelStatusCheckSchedulerDelay = maybe 120 (.getSeconds) exotelStatusCheckSchedulerDelay,
            feedbackAlertRatingThreshold = fromMaybe 3 feedbackAlertRatingThreshold,
            hardLimitForSafetyJobs = fromMaybe 21600 ((.getSeconds) <$> hardLimitForSafetyJobs),
            incidentReportSupport = fromMaybe False incidentReportSupport,
            isAvoidToll = isAvoidToll,
            ivrTriggerDelay = fromMaybe 3000 (Kernel.Utils.Common.secondsToNominalDiffTime <$> ivrTriggerDelay),
            kaptureConfig = kaptureConfig,
            kaptureQueue = kaptureQueue,
            localPoliceNumber = localPoliceNumber,
            makeMultiModalSearch = fromMaybe False makeMultiModalSearch,
            maximumWalkDistance = fromMaybe 600 maximumWalkDistance,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metroBookingAllowed = metroBookingAllowed,
            minRidesToBlock = minRidesToBlock,
            minRidesToShowCancellationRate = minRidesToShowCancellationRate,
            payoutBatchDelay = fromMaybe 10 (Kernel.Utils.Common.secondsToNominalDiffTime <$> payoutBatchDelay),
            payoutBatchSize = payoutBatchSize,
            payoutReferralProgram = fromMaybe False payoutReferralProgram,
            payoutReferralStartDate = payoutReferralStartDate_,
            placeNameCacheExpiryDays = placeNameCacheExpiryDays,
            policeTriggerDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> policeTriggerDelay),
            postRideSafetyNotificationDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> postRideSafetyNotificationDelay),
            safetyCheckEndTime = safetyCheckEndTime,
            safetyCheckStartTime = safetyCheckStartTime,
            sensitiveWords = sensitiveWords,
            sensitiveWordsForExactMatch = sensitiveWordsForExactMatch,
            settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
            specialZoneRadius = specialZoneRadius,
            thresholdCancellationPercentageToBlock = thresholdCancellationPercentageToBlock,
            timeDiffFromUtc = timeDiffFromUtc,
            trackingShortUrlPattern = trackingShortUrlPattern,
            useUserSettingsForSafetyIVR = fromMaybe False useUserSettingsForSafetyIVR,
            videoFileSizeUpperLimit = videoFileSizeUpperLimit,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  toTType' (Domain.Types.RiderConfig.RiderConfig {..}) = do
    Beam.RiderConfigT
      { Beam.appUrl = appUrl,
        Beam.autoSendBookingDetailsViaWhatsapp = autoSendBookingDetailsViaWhatsapp,
        Beam.autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
        Beam.avgSpeedInKmPerHr = Just avgSpeedInKmPerHr,
        Beam.bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
        Beam.cancellationPaymentDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) cancellationPaymentDelay,
        Beam.collectAutoCompleteData = collectAutoCompleteData,
        Beam.collectMMIRouteData = collectMMIRouteData,
        Beam.csAlertTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) csAlertTriggerDelay,
        Beam.cxAgentDetails = cxAgentDetails,
        Beam.distanceWeightage = distanceWeightage,
        Beam.driverReferredSearchReqExpiry = driverReferredSearchReqExpiry,
        Beam.dynamicLogicUpdatePassword = Just dynamicLogicUpdatePassword,
        Beam.emailOtpConfig = emailOtpConfig,
        Beam.enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
        Beam.enableLocalPoliceSupport = enableLocalPoliceSupport,
        Beam.enableSupportForSafety = enableSupportForSafety,
        Beam.executePaymentDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) executePaymentDelay,
        Beam.exotelAppIdMapping = exotelAppIdMapping,
        Beam.exotelStatusCheckSchedulerDelay = Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay,
        Beam.feedbackAlertRatingThreshold = Just feedbackAlertRatingThreshold,
        Beam.hardLimitForSafetyJobs = Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs,
        Beam.incidentReportSupport = Just incidentReportSupport,
        Beam.isAvoidToll = isAvoidToll,
        Beam.ivrTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay,
        Beam.kaptureConfig = kaptureConfig,
        Beam.kaptureQueue = kaptureQueue,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.makeMultiModalSearch = Just makeMultiModalSearch,
        Beam.maximumWalkDistance = Just maximumWalkDistance,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metroBookingAllowed = metroBookingAllowed,
        Beam.minRidesToBlock = minRidesToBlock,
        Beam.minRidesToShowCancellationRate = minRidesToShowCancellationRate,
        Beam.payoutBatchDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) payoutBatchDelay,
        Beam.payoutBatchSize = payoutBatchSize,
        Beam.payoutReferralProgram = Just payoutReferralProgram,
        Beam.payoutReferralStartDate = Just payoutReferralStartDate,
        Beam.placeNameCacheExpiryDays = placeNameCacheExpiryDays,
        Beam.policeTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay,
        Beam.postRideSafetyNotificationDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) postRideSafetyNotificationDelay,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.sensitiveWords = sensitiveWords,
        Beam.sensitiveWordsForExactMatch = sensitiveWordsForExactMatch,
        Beam.settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
        Beam.specialZoneRadius = specialZoneRadius,
        Beam.thresholdCancellationPercentageToBlock = thresholdCancellationPercentageToBlock,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.useUserSettingsForSafetyIVR = Just useUserSettingsForSafetyIVR,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
