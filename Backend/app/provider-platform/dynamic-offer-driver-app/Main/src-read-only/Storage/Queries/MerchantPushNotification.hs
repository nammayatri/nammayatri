{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPushNotification where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPushNotification
import qualified Domain.Types.Trip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantPushNotification as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPushNotification.MerchantPushNotification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantPushNotification.MerchantPushNotification] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.MerchantPushNotification.MerchantPushNotification])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findAllByMerchantOpCityIdAndMessageKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> m [Domain.Types.MerchantPushNotification.MerchantPushNotification])
findAllByMerchantOpCityIdAndMessageKey merchantOperatingCityId key = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.key $ Se.Eq key
        ]
    ]

findAllByMerchantOpCityIdAndMessageKeyAndTripCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Trip.TripCategory -> m [Domain.Types.MerchantPushNotification.MerchantPushNotification])
findAllByMerchantOpCityIdAndMessageKeyAndTripCategory merchantOperatingCityId key tripCategory = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.key $ Se.Eq key,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantPushNotification.MerchantPushNotification -> m (Maybe Domain.Types.MerchantPushNotification.MerchantPushNotification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPushNotification.MerchantPushNotification -> m ())
updateByPrimaryKey (Domain.Types.MerchantPushNotification.MerchantPushNotification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.body body,
      Se.Set Beam.fcmNotificationType fcmNotificationType,
      Se.Set Beam.fcmSubCategory fcmSubCategory,
      Se.Set Beam.key key,
      Se.Set Beam.language language,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.title title,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantPushNotification Domain.Types.MerchantPushNotification.MerchantPushNotification where
  fromTType' (Beam.MerchantPushNotificationT {..}) = do
    pure $
      Just
        Domain.Types.MerchantPushNotification.MerchantPushNotification
          { body = body,
            fcmNotificationType = fcmNotificationType,
            fcmSubCategory = fcmSubCategory,
            id = Kernel.Types.Id.Id id,
            key = key,
            language = language,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            title = title,
            tripCategory = tripCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantPushNotification Domain.Types.MerchantPushNotification.MerchantPushNotification where
  toTType' (Domain.Types.MerchantPushNotification.MerchantPushNotification {..}) = do
    Beam.MerchantPushNotificationT
      { Beam.body = body,
        Beam.fcmNotificationType = fcmNotificationType,
        Beam.fcmSubCategory = fcmSubCategory,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.key = key,
        Beam.language = language,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.title = title,
        Beam.tripCategory = tripCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
