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
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RiderConfig.RiderConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderConfig.RiderConfig -> m ())
updateByPrimaryKey (Domain.Types.RiderConfig.RiderConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enableLocalPoliceSupport enableLocalPoliceSupport,
      Se.Set Beam.localPoliceNumber localPoliceNumber,
      Se.Set Beam.enableSupportForSafety enableSupportForSafety,
      Se.Set Beam.videoFileSizeUpperLimit videoFileSizeUpperLimit,
      Se.Set Beam.timeDiffFromUtc timeDiffFromUtc,
      Se.Set Beam.enableEmergencyContactAddedMessage enableEmergencyContactAddedMessage,
      Se.Set Beam.safetyCheckStartTime safetyCheckStartTime,
      Se.Set Beam.safetyCheckEndTime safetyCheckEndTime,
      Se.Set Beam.trackingShortUrlPattern trackingShortUrlPattern,
      Se.Set Beam.specialZoneRadius specialZoneRadius,
      Se.Set Beam.appUrl appUrl,
      Se.Set Beam.collectAutoCompleteData collectAutoCompleteData,
      Se.Set Beam.distanceWeightage distanceWeightage,
      Se.Set Beam.collectMMIRouteData collectMMIRouteData,
      Se.Set Beam.isAvoidToll isAvoidToll,
      Se.Set Beam.autoUnblockSafetyCenterAfterDays autoUnblockSafetyCenterAfterDays,
      Se.Set Beam.placeNameCacheExpiryDays placeNameCacheExpiryDays,
      Se.Set Beam.bookingSyncStatusCallSecondsDiffThreshold bookingSyncStatusCallSecondsDiffThreshold,
      Se.Set Beam.kaptureQueue kaptureQueue,
      Se.Set Beam.kaptureConfig kaptureConfig,
      Se.Set Beam.emailOtpConfig emailOtpConfig,
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
          { merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            enableLocalPoliceSupport = enableLocalPoliceSupport,
            localPoliceNumber = localPoliceNumber,
            enableSupportForSafety = enableSupportForSafety,
            videoFileSizeUpperLimit = videoFileSizeUpperLimit,
            timeDiffFromUtc = timeDiffFromUtc,
            enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
            safetyCheckStartTime = safetyCheckStartTime,
            safetyCheckEndTime = safetyCheckEndTime,
            trackingShortUrlPattern = trackingShortUrlPattern,
            specialZoneRadius = specialZoneRadius,
            appUrl = appUrl,
            collectAutoCompleteData = collectAutoCompleteData,
            distanceWeightage = distanceWeightage,
            collectMMIRouteData = collectMMIRouteData,
            isAvoidToll = isAvoidToll,
            autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
            placeNameCacheExpiryDays = placeNameCacheExpiryDays,
            bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
            kaptureQueue = kaptureQueue,
            kaptureConfig = kaptureConfig,
            emailOtpConfig = emailOtpConfig,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  toTType' (Domain.Types.RiderConfig.RiderConfig {..}) = do
    Beam.RiderConfigT
      { Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.enableLocalPoliceSupport = enableLocalPoliceSupport,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.enableSupportForSafety = enableSupportForSafety,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.specialZoneRadius = specialZoneRadius,
        Beam.appUrl = appUrl,
        Beam.collectAutoCompleteData = collectAutoCompleteData,
        Beam.distanceWeightage = distanceWeightage,
        Beam.collectMMIRouteData = collectMMIRouteData,
        Beam.isAvoidToll = isAvoidToll,
        Beam.autoUnblockSafetyCenterAfterDays = autoUnblockSafetyCenterAfterDays,
        Beam.placeNameCacheExpiryDays = placeNameCacheExpiryDays,
        Beam.bookingSyncStatusCallSecondsDiffThreshold = bookingSyncStatusCallSecondsDiffThreshold,
        Beam.kaptureQueue = kaptureQueue,
        Beam.kaptureConfig = kaptureConfig,
        Beam.emailOtpConfig = emailOtpConfig,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
