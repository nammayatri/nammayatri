{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderConfig.RiderConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RiderConfig.RiderConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findByMerchantOperatingCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findByPrimaryKey (Kernel.Types.Id.Id merchantOperatingCityId) = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]]

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
      Se.Set Beam.isAvoidToll isAvoidToll,
      Se.Set Beam.kaptureQueue kaptureQueue,
      Se.Set Beam.localPoliceNumber localPoliceNumber,
      Se.Set Beam.placeNameCacheExpiryDays placeNameCacheExpiryDays,
      Se.Set Beam.safetyCheckEndTime safetyCheckEndTime,
      Se.Set Beam.safetyCheckStartTime safetyCheckStartTime,
      Se.Set Beam.shouldBlockedBySameDeviceToken shouldBlockedBySameDeviceToken,
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
            isAvoidToll = isAvoidToll,
            kaptureQueue = kaptureQueue,
            localPoliceNumber = localPoliceNumber,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            placeNameCacheExpiryDays = placeNameCacheExpiryDays,
            safetyCheckEndTime = safetyCheckEndTime,
            safetyCheckStartTime = safetyCheckStartTime,
            shouldBlockedBySameDeviceToken = shouldBlockedBySameDeviceToken,
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
        Beam.isAvoidToll = isAvoidToll,
        Beam.kaptureQueue = kaptureQueue,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.placeNameCacheExpiryDays = placeNameCacheExpiryDays,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.shouldBlockedBySameDeviceToken = shouldBlockedBySameDeviceToken,
        Beam.specialZoneRadius = specialZoneRadius,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
