{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NotificationSoundsConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.NotificationSoundsConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Notification.Interface.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NotificationSoundsConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig] -> m ())
createMany = traverse_ create

findByNotificationType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.External.Notification.Interface.Types.Category -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig))
findByNotificationType notificationType merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.notificationType $ Se.Eq notificationType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.External.Notification.Interface.Types.Category -> m (Maybe Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig))
findByPrimaryKey merchantOperatingCityId notificationType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.notificationType $ Se.Eq notificationType
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig -> m ())
updateByPrimaryKey (Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blindSound blindSound,
      Se.Set Beam.defaultSound defaultSound,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.notificationType $ Se.Eq notificationType]]

instance FromTType' Beam.NotificationSoundsConfig Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig where
  fromTType' (Beam.NotificationSoundsConfigT {..}) = do
    pure $
      Just
        Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig
          { blindSound = blindSound,
            defaultSound = defaultSound,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            notificationType = notificationType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.NotificationSoundsConfig Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig where
  toTType' (Domain.Types.NotificationSoundsConfig.NotificationSoundsConfig {..}) = do
    Beam.NotificationSoundsConfigT
      { Beam.blindSound = blindSound,
        Beam.defaultSound = defaultSound,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notificationType = notificationType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
