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
      Se.Set Beam.autoUnblockSafetyCenterAfterDays autoUnblockSafetyCenterAfterDays,
      Se.Set Beam.bookingSyncStatusCallSecondsDiffThreshold bookingSyncStatusCallSecondsDiffThreshold,
      Se.Set Beam.collectAutoCompleteData collectAutoCompleteData,
      Se.Set Beam.collectMMIRouteData collectMMIRouteData,
      Se.Set Beam.distanceWeightage distanceWeightage,
      Se.Set Beam.emailOtpConfig emailOtpConfig,
      Se.Set Beam.enableEmergencyContactAddedMessage enableEmergencyContactAddedMessage,
      Se.Set Beam.enableLocalPoliceSupport enableLocalPoliceSupport,
      Se.Set Beam.enableSupportForSafety enableSupportForSafety,
      Se.Set Beam.exotelAppIdMapping exotelAppIdMapping,
      Se.Set Beam.exotelStatusCheckSchedulerDelay (Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay),
      Se.Set Beam.hardLimitForSafetyJobs (Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs),
      Se.Set Beam.incidentReportSupport (Just incidentReportSupport),
      Se.Set Beam.isAvoidToll isAvoidToll,
      Se.Set Beam.ivrTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay),
      Se.Set Beam.kaptureConfig kaptureConfig,
      Se.Set Beam.kaptureQueue kaptureQueue,
      Se.Set Beam.localPoliceNumber localPoliceNumber,
      Se.Set Beam.placeNameCacheExpiryDays placeNameCacheExpiryDays,
      Se.Set Beam.policeTriggerDelay ((Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay),
      Se.Set Beam.safetyCheckEndTime safetyCheckEndTime,
      Se.Set Beam.safetyCheckStartTime safetyCheckStartTime,
      Se.Set Beam.settleCancellationFeeBeforeNextRide settleCancellationFeeBeforeNextRide,
      Se.Set Beam.specialZoneRadius specialZoneRadius,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.trackingShortUrlPattern trackingShortUrlPattern,
      Se.Set Beam.videoFileSizeUpperLimit videoFileSizeUpperLimit,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  fromTType' (Beam.RiderConfigT {..}) = do
    pure $
      Just
        Domain.Types.RiderConfig.RiderConfig
          { appUrl = appUrl,
            autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
            bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
            collectAutoCompleteData = collectAutoCompleteData,
            collectMMIRouteData = collectMMIRouteData,
            distanceWeightage = distanceWeightage,
            emailOtpConfig = emailOtpConfig,
            enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
            enableLocalPoliceSupport = enableLocalPoliceSupport,
            enableSupportForSafety = enableSupportForSafety,
            exotelAppIdMapping = exotelAppIdMapping,
            exotelStatusCheckSchedulerDelay = maybe 120 (.getSeconds) exotelStatusCheckSchedulerDelay,
            hardLimitForSafetyJobs = fromMaybe 21600 ((.getSeconds) <$> hardLimitForSafetyJobs),
            incidentReportSupport = fromMaybe False incidentReportSupport,
            isAvoidToll = isAvoidToll,
            ivrTriggerDelay = fromMaybe 3000 (Kernel.Utils.Common.secondsToNominalDiffTime <$> ivrTriggerDelay),
            kaptureConfig = kaptureConfig,
            kaptureQueue = kaptureQueue,
            localPoliceNumber = localPoliceNumber,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            placeNameCacheExpiryDays = placeNameCacheExpiryDays,
            policeTriggerDelay = fromMaybe 60 (Kernel.Utils.Common.secondsToNominalDiffTime <$> policeTriggerDelay),
            safetyCheckEndTime = safetyCheckEndTime,
            safetyCheckStartTime = safetyCheckStartTime,
            settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
            specialZoneRadius = specialZoneRadius,
            timeDiffFromUtc = timeDiffFromUtc,
            trackingShortUrlPattern = trackingShortUrlPattern,
            videoFileSizeUpperLimit = videoFileSizeUpperLimit,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  toTType' (Domain.Types.RiderConfig.RiderConfig {..}) = do
    Beam.RiderConfigT
      { Beam.appUrl = appUrl,
        Beam.autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
        Beam.bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
        Beam.collectAutoCompleteData = collectAutoCompleteData,
        Beam.collectMMIRouteData = collectMMIRouteData,
        Beam.distanceWeightage = distanceWeightage,
        Beam.emailOtpConfig = emailOtpConfig,
        Beam.enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
        Beam.enableLocalPoliceSupport = enableLocalPoliceSupport,
        Beam.enableSupportForSafety = enableSupportForSafety,
        Beam.exotelAppIdMapping = exotelAppIdMapping,
        Beam.exotelStatusCheckSchedulerDelay = Just $ Kernel.Types.Common.Seconds exotelStatusCheckSchedulerDelay,
        Beam.hardLimitForSafetyJobs = Just $ Kernel.Types.Common.Seconds hardLimitForSafetyJobs,
        Beam.incidentReportSupport = Just incidentReportSupport,
        Beam.isAvoidToll = isAvoidToll,
        Beam.ivrTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) ivrTriggerDelay,
        Beam.kaptureConfig = kaptureConfig,
        Beam.kaptureQueue = kaptureQueue,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.placeNameCacheExpiryDays = placeNameCacheExpiryDays,
        Beam.policeTriggerDelay = (Just . Kernel.Utils.Common.nominalDiffTimeToSeconds) policeTriggerDelay,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.settleCancellationFeeBeforeNextRide = settleCancellationFeeBeforeNextRide,
        Beam.specialZoneRadius = specialZoneRadius,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
