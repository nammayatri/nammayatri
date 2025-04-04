{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderConfig where

import qualified Data.Aeson
import qualified Data.Text
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
import qualified Kernel.Utils.JSON
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
      Se.Set Beam.blockedUntilInMins blockedUntilInMins,
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
      Se.Set Beam.excludedVehicleVariants excludedVehicleVariants,
      Se.Set Beam.executePaymentDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) executePaymentDelay),
      Se.Set Beam.exotelAppIdMapping exotelAppIdMapping,
      Se.Set Beam.exotelStatusCheckSchedulerDelay (Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay),
      Se.Set Beam.fareCacheInterCitySearchLocations (fareCacheInterCitySearchLocations >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.fareCacheRentalsConfig (fareCacheRentalsConfig >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.feedbackAlertRatingThreshold (Just feedbackAlertRatingThreshold),
      Se.Set Beam.hardLimitForSafetyJobs (Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs),
      Se.Set Beam.incidentReportSupport (Just incidentReportSupport),
      Se.Set Beam.intercitySearchLocations intercitySearchLocations,
      Se.Set Beam.isAvoidToll isAvoidToll,
      Se.Set Beam.isDeviceIdCheckDisabled isDeviceIdCheckDisabled,
      Se.Set Beam.isFirstReferredRideEnabled (Just isFirstReferredRideEnabled),
      Se.Set Beam.ivrTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay),
      Se.Set Beam.kaptureConfig kaptureConfig,
      Se.Set Beam.kaptureQueue kaptureQueue,
      Se.Set Beam.localPoliceNumber localPoliceNumber,
      Se.Set Beam.makeMultiModalSearch (Just makeMultiModalSearch),
      Se.Set Beam.maxAllowedPublicTransportLegs (Just maxAllowedPublicTransportLegs),
      Se.Set Beam.maximumWalkDistance (Just maximumWalkDistance),
      Se.Set Beam.metroBookingAllowed metroBookingAllowed,
      Se.Set Beam.minRidesToBlock minRidesToBlock,
      Se.Set Beam.minRidesToShowCancellationRate minRidesToShowCancellationRate,
      Se.Set Beam.minimumWalkDistance (Just minimumWalkDistance),
      Se.Set Beam.multimodalTesting (Just multimodalTesting),
      Se.Set Beam.nearByDriverRingBucketCfg (nearByDriverRingBucketCfg >>= Just . Data.Aeson.toJSON),
      Se.Set Beam.payoutBatchDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) payoutBatchDelay),
      Se.Set Beam.payoutBatchSize payoutBatchSize,
      Se.Set Beam.payoutReferralProgram (Just payoutReferralProgram),
      Se.Set Beam.payoutReferralStartDate (Just payoutReferralStartDate),
      Se.Set Beam.payoutReferralThresholdPerDay (Just payoutReferralThresholdPerDay),
      Se.Set Beam.payoutReferralThresholdPerMonth (Just payoutReferralThresholdPerMonth),
      Se.Set Beam.permissibleModes permissibleModes,
      Se.Set Beam.placeNameCacheExpiryDays placeNameCacheExpiryDays,
      Se.Set Beam.policeTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay),
      Se.Set Beam.postRideSafetyNotificationDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) postRideSafetyNotificationDelay),
      Se.Set Beam.rentalsConfig rentalsConfig,
      Se.Set Beam.safetyCheckEndTime safetyCheckEndTime,
      Se.Set Beam.safetyCheckStartTime safetyCheckStartTime,
      Se.Set Beam.sensitiveWords sensitiveWords,
      Se.Set Beam.sensitiveWordsForExactMatch sensitiveWordsForExactMatch,
      Se.Set Beam.settleCancellationFeeBeforeNextRide settleCancellationFeeBeforeNextRide,
      Se.Set Beam.specialZoneRadius specialZoneRadius,
      Se.Set Beam.straightLineThreshold (Just straightLineThreshold),
      Se.Set Beam.suburbanBookingAllowed suburbanBookingAllowed,
      Se.Set Beam.thresholdCancellationPercentageToBlock thresholdCancellationPercentageToBlock,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.trackingShortUrlPattern trackingShortUrlPattern,
      Se.Set Beam.useUserSettingsForSafetyIVR (Just useUserSettingsForSafetyIVR),
      Se.Set Beam.variantListForNearByReq (variantListForNearByReq >>= Just . map show),
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
            blockedUntilInMins = blockedUntilInMins,
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
            excludedVehicleVariants = excludedVehicleVariants,
            executePaymentDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> executePaymentDelay),
            exotelAppIdMapping = exotelAppIdMapping,
            exotelStatusCheckSchedulerDelay = maybe 120 (.getSeconds) exotelStatusCheckSchedulerDelay,
            fareCacheInterCitySearchLocations = fareCacheInterCitySearchLocations >>= Kernel.Utils.JSON.valueToMaybe,
            fareCacheRentalsConfig = fareCacheRentalsConfig >>= Kernel.Utils.JSON.valueToMaybe,
            feedbackAlertRatingThreshold = fromMaybe 3 feedbackAlertRatingThreshold,
            hardLimitForSafetyJobs = fromMaybe 21600 ((.getSeconds) <$> hardLimitForSafetyJobs),
            incidentReportSupport = fromMaybe False incidentReportSupport,
            intercitySearchLocations = intercitySearchLocations,
            isAvoidToll = isAvoidToll,
            isDeviceIdCheckDisabled = isDeviceIdCheckDisabled,
            isFirstReferredRideEnabled = fromMaybe False isFirstReferredRideEnabled,
            ivrTriggerDelay = fromMaybe 3000 (Kernel.Utils.Common.secondsToNominalDiffTime <$> ivrTriggerDelay),
            kaptureConfig = kaptureConfig,
            kaptureQueue = kaptureQueue,
            localPoliceNumber = localPoliceNumber,
            makeMultiModalSearch = fromMaybe False makeMultiModalSearch,
            maxAllowedPublicTransportLegs = fromMaybe 2 maxAllowedPublicTransportLegs,
            maximumWalkDistance = fromMaybe 600 maximumWalkDistance,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            metroBookingAllowed = metroBookingAllowed,
            minRidesToBlock = minRidesToBlock,
            minRidesToShowCancellationRate = minRidesToShowCancellationRate,
            minimumWalkDistance = fromMaybe 100 minimumWalkDistance,
            multimodalTesting = fromMaybe False multimodalTesting,
            nearByDriverRingBucketCfg = nearByDriverRingBucketCfg >>= Kernel.Utils.JSON.valueToMaybe,
            payoutBatchDelay = fromMaybe 10 (Kernel.Utils.Common.secondsToNominalDiffTime <$> payoutBatchDelay),
            payoutBatchSize = payoutBatchSize,
            payoutReferralProgram = fromMaybe False payoutReferralProgram,
            payoutReferralStartDate = payoutReferralStartDate_,
            payoutReferralThresholdPerDay = fromMaybe 5 payoutReferralThresholdPerDay,
            payoutReferralThresholdPerMonth = fromMaybe 50 payoutReferralThresholdPerMonth,
            permissibleModes = permissibleModes,
            placeNameCacheExpiryDays = placeNameCacheExpiryDays,
            policeTriggerDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> policeTriggerDelay),
            postRideSafetyNotificationDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> postRideSafetyNotificationDelay),
            rentalsConfig = rentalsConfig,
            safetyCheckEndTime = safetyCheckEndTime,
            safetyCheckStartTime = safetyCheckStartTime,
            sensitiveWords = sensitiveWords,
            sensitiveWordsForExactMatch = sensitiveWordsForExactMatch,
            settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
            specialZoneRadius = specialZoneRadius,
            straightLineThreshold = fromMaybe 300 straightLineThreshold,
            suburbanBookingAllowed = suburbanBookingAllowed,
            thresholdCancellationPercentageToBlock = thresholdCancellationPercentageToBlock,
            timeDiffFromUtc = timeDiffFromUtc,
            trackingShortUrlPattern = trackingShortUrlPattern,
            useUserSettingsForSafetyIVR = fromMaybe False useUserSettingsForSafetyIVR,
            variantListForNearByReq = variantListForNearByReq >>= traverse (readMaybe . Data.Text.unpack),
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
        Beam.blockedUntilInMins = blockedUntilInMins,
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
        Beam.excludedVehicleVariants = excludedVehicleVariants,
        Beam.executePaymentDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) executePaymentDelay,
        Beam.exotelAppIdMapping = exotelAppIdMapping,
        Beam.exotelStatusCheckSchedulerDelay = Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay,
        Beam.fareCacheInterCitySearchLocations = fareCacheInterCitySearchLocations >>= Just . Data.Aeson.toJSON,
        Beam.fareCacheRentalsConfig = fareCacheRentalsConfig >>= Just . Data.Aeson.toJSON,
        Beam.feedbackAlertRatingThreshold = Just feedbackAlertRatingThreshold,
        Beam.hardLimitForSafetyJobs = Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs,
        Beam.incidentReportSupport = Just incidentReportSupport,
        Beam.intercitySearchLocations = intercitySearchLocations,
        Beam.isAvoidToll = isAvoidToll,
        Beam.isDeviceIdCheckDisabled = isDeviceIdCheckDisabled,
        Beam.isFirstReferredRideEnabled = Just isFirstReferredRideEnabled,
        Beam.ivrTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay,
        Beam.kaptureConfig = kaptureConfig,
        Beam.kaptureQueue = kaptureQueue,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.makeMultiModalSearch = Just makeMultiModalSearch,
        Beam.maxAllowedPublicTransportLegs = Just maxAllowedPublicTransportLegs,
        Beam.maximumWalkDistance = Just maximumWalkDistance,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.metroBookingAllowed = metroBookingAllowed,
        Beam.minRidesToBlock = minRidesToBlock,
        Beam.minRidesToShowCancellationRate = minRidesToShowCancellationRate,
        Beam.minimumWalkDistance = Just minimumWalkDistance,
        Beam.multimodalTesting = Just multimodalTesting,
        Beam.nearByDriverRingBucketCfg = nearByDriverRingBucketCfg >>= Just . Data.Aeson.toJSON,
        Beam.payoutBatchDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) payoutBatchDelay,
        Beam.payoutBatchSize = payoutBatchSize,
        Beam.payoutReferralProgram = Just payoutReferralProgram,
        Beam.payoutReferralStartDate = Just payoutReferralStartDate,
        Beam.payoutReferralThresholdPerDay = Just payoutReferralThresholdPerDay,
        Beam.payoutReferralThresholdPerMonth = Just payoutReferralThresholdPerMonth,
        Beam.permissibleModes = permissibleModes,
        Beam.placeNameCacheExpiryDays = placeNameCacheExpiryDays,
        Beam.policeTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay,
        Beam.postRideSafetyNotificationDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) postRideSafetyNotificationDelay,
        Beam.rentalsConfig = rentalsConfig,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.sensitiveWords = sensitiveWords,
        Beam.sensitiveWordsForExactMatch = sensitiveWordsForExactMatch,
        Beam.settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
        Beam.specialZoneRadius = specialZoneRadius,
        Beam.straightLineThreshold = Just straightLineThreshold,
        Beam.suburbanBookingAllowed = suburbanBookingAllowed,
        Beam.thresholdCancellationPercentageToBlock = thresholdCancellationPercentageToBlock,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.useUserSettingsForSafetyIVR = Just useUserSettingsForSafetyIVR,
        Beam.variantListForNearByReq = variantListForNearByReq >>= Just . map show,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
