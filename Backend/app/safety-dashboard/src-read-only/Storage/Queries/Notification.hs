{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Notification where

import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Notification
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Notification as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Notification.Notification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Notification.Notification] -> m ())
createMany = traverse_ create

findByMerchantIdAndreadStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Notification.NotificationCategory -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m (Maybe Domain.Types.Notification.Notification))
findByMerchantIdAndreadStatus notificationCategory readStatus merchantId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.notificationCategory $ Se.Eq notificationCategory,
          Se.Is Beam.readStatus $ Se.Eq readStatus,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)
        ]
    ]

findByReceiverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.Notification.Notification])
findByReceiverId limit offset receiverId = do findAllWithOptionsKV [Se.And [Se.Is Beam.receiverId $ Se.Eq receiverId]] (Se.Desc Beam.createdAt) limit offset

findByReceiverIdAndId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Notification.Notification -> m (Maybe Domain.Types.Notification.Notification))
findByReceiverIdAndId receiverId id = do findOneWithKV [Se.And [Se.Is Beam.receiverId $ Se.Eq receiverId, Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByReceiverIdAndReadStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m [Domain.Types.Notification.Notification])
findByReceiverIdAndReadStatus limit offset receiverId readStatus = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.receiverId $ Se.Eq receiverId,
          Se.Is Beam.readStatus $ Se.Eq readStatus
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateReadStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Notification.Notification -> m ())
updateReadStatusById readStatus id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.readStatus readStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Notification.Notification -> m (Maybe Domain.Types.Notification.Notification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Notification.Notification -> m ())
updateByPrimaryKey (Domain.Types.Notification.Notification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.metadata metadata,
      Se.Set Beam.notificationCategory notificationCategory,
      Se.Set Beam.notificationCount notificationCount,
      Se.Set Beam.readStatus readStatus,
      Se.Set Beam.receiverId receiverId,
      Se.Set Beam.senderId senderId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Notification Domain.Types.Notification.Notification where
  fromTType' (Beam.NotificationT {..}) = do
    pure $
      Just
        Domain.Types.Notification.Notification
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantShortId = merchantShortId,
            metadata = metadata,
            notificationCategory = notificationCategory,
            notificationCount = notificationCount,
            readStatus = readStatus,
            receiverId = receiverId,
            senderId = senderId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId
          }

instance ToTType' Beam.Notification Domain.Types.Notification.Notification where
  toTType' (Domain.Types.Notification.Notification {..}) = do
    Beam.NotificationT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantShortId = merchantShortId,
        Beam.metadata = metadata,
        Beam.notificationCategory = notificationCategory,
        Beam.notificationCount = notificationCount,
        Beam.readStatus = readStatus,
        Beam.receiverId = receiverId,
        Beam.senderId = senderId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId
      }
