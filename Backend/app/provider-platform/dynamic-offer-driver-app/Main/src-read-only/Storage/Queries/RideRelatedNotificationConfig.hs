{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RideRelatedNotificationConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.RideRelatedNotificationConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.RideRelatedNotificationConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOperatingCityIdAndTimeDiffEvent ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.RideRelatedNotificationConfig.TimeDiffEvent -> m [Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig])
findAllByMerchantOperatingCityIdAndTimeDiffEvent merchantOperatingCityId timeDiffEvent = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeDiffEvent $ Se.Eq timeDiffEvent
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig))
findByPrimaryKey id merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig -> m ())
updateByPrimaryKey (Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.eventTime eventTime,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.notificationKey notificationKey,
      Se.Set Beam.notificationType notificationType,
      Se.Set Beam.onBookingStatus onBookingStatus,
      Se.Set Beam.onScheduledBooking onScheduledBooking,
      Se.Set Beam.onlyIfOffline onlyIfOffline,
      Se.Set Beam.timeDiff (Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff),
      Se.Set Beam.timeDiffEvent timeDiffEvent,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.RideRelatedNotificationConfig Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig where
  fromTType' (Beam.RideRelatedNotificationConfigT {..}) = do
    pure $
      Just
        Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig
          { eventTime = eventTime,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            notificationKey = notificationKey,
            notificationType = notificationType,
            onBookingStatus = onBookingStatus,
            onScheduledBooking = onScheduledBooking,
            onlyIfOffline = onlyIfOffline,
            timeDiff = Kernel.Utils.Common.secondsToNominalDiffTime timeDiff,
            timeDiffEvent = timeDiffEvent,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RideRelatedNotificationConfig Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig where
  toTType' (Domain.Types.RideRelatedNotificationConfig.RideRelatedNotificationConfig {..}) = do
    Beam.RideRelatedNotificationConfigT
      { Beam.eventTime = eventTime,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notificationKey = notificationKey,
        Beam.notificationType = notificationType,
        Beam.onBookingStatus = onBookingStatus,
        Beam.onScheduledBooking = onScheduledBooking,
        Beam.onlyIfOffline = onlyIfOffline,
        Beam.timeDiff = Kernel.Utils.Common.nominalDiffTimeToSeconds timeDiff,
        Beam.timeDiffEvent = timeDiffEvent,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
