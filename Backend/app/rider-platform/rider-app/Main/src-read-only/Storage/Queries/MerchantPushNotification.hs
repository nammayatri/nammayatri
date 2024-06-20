{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantPushNotification where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantPushNotification
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

findByMerchantOpCityIdAndMessageKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> m (Maybe Domain.Types.MerchantPushNotification.MerchantPushNotification))
findByMerchantOpCityIdAndMessageKey merchantOperatingCityId key = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.key $ Se.Eq key
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantPushNotification.MerchantPushNotification))
findByPrimaryKey key merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.key $ Se.Eq key, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantPushNotification.MerchantPushNotification -> m ())
updateByPrimaryKey (Domain.Types.MerchantPushNotification.MerchantPushNotification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.title title,
      Se.Set Beam.body body,
      Se.Set Beam.language language,
      Se.Set Beam.fcmNotificationType fcmNotificationType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.key $ Se.Eq key, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.MerchantPushNotification Domain.Types.MerchantPushNotification.MerchantPushNotification where
  fromTType' (Beam.MerchantPushNotificationT {..}) = do
    pure $
      Just
        Domain.Types.MerchantPushNotification.MerchantPushNotification
          { key = key,
            title = title,
            body = body,
            language = language,
            fcmNotificationType = fcmNotificationType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantPushNotification Domain.Types.MerchantPushNotification.MerchantPushNotification where
  toTType' (Domain.Types.MerchantPushNotification.MerchantPushNotification {..}) = do
    Beam.MerchantPushNotificationT
      { Beam.key = key,
        Beam.title = title,
        Beam.body = body,
        Beam.language = language,
        Beam.fcmNotificationType = fcmNotificationType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
