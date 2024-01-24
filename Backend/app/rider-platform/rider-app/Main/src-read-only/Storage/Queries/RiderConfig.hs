{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RiderConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.RiderConfig.RiderConfig -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.RiderConfig.RiderConfig] -> m ()
createMany = traverse_ createWithKV

findByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe (Domain.Types.RiderConfig.RiderConfig))
findByMerchantOperatingCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findOneWithKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe (Domain.Types.RiderConfig.RiderConfig))
findByPrimaryKey (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.RiderConfig.RiderConfig -> m ()
updateByPrimaryKey Domain.Types.RiderConfig.RiderConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.appUrl $ appUrl,
      Se.Set Beam.enableEmergencyContactAddedMessage $ enableEmergencyContactAddedMessage,
      Se.Set Beam.enableLocalPoliceSupport $ enableLocalPoliceSupport,
      Se.Set Beam.enableSupportForSafety $ enableSupportForSafety,
      Se.Set Beam.localPoliceNumber $ localPoliceNumber,
      Se.Set Beam.safetyCheckEndTime $ safetyCheckEndTime,
      Se.Set Beam.safetyCheckStartTime $ safetyCheckStartTime,
      Se.Set Beam.timeDiffFromUtc $ timeDiffFromUtc,
      Se.Set Beam.trackingShortUrlPattern $ trackingShortUrlPattern,
      Se.Set Beam.videoFileSizeUpperLimit $ videoFileSizeUpperLimit,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

instance FromTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  fromTType' Beam.RiderConfigT {..} = do
    pure $
      Just
        Domain.Types.RiderConfig.RiderConfig
          { appUrl = appUrl,
            enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
            enableLocalPoliceSupport = enableLocalPoliceSupport,
            enableSupportForSafety = enableSupportForSafety,
            localPoliceNumber = localPoliceNumber,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            safetyCheckEndTime = safetyCheckEndTime,
            safetyCheckStartTime = safetyCheckStartTime,
            timeDiffFromUtc = timeDiffFromUtc,
            trackingShortUrlPattern = trackingShortUrlPattern,
            videoFileSizeUpperLimit = videoFileSizeUpperLimit,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderConfig Domain.Types.RiderConfig.RiderConfig where
  toTType' Domain.Types.RiderConfig.RiderConfig {..} = do
    Beam.RiderConfigT
      { Beam.appUrl = appUrl,
        Beam.enableEmergencyContactAddedMessage = enableEmergencyContactAddedMessage,
        Beam.enableLocalPoliceSupport = enableLocalPoliceSupport,
        Beam.enableSupportForSafety = enableSupportForSafety,
        Beam.localPoliceNumber = localPoliceNumber,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.safetyCheckEndTime = safetyCheckEndTime,
        Beam.safetyCheckStartTime = safetyCheckStartTime,
        Beam.timeDiffFromUtc = timeDiffFromUtc,
        Beam.trackingShortUrlPattern = trackingShortUrlPattern,
        Beam.videoFileSizeUpperLimit = videoFileSizeUpperLimit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
