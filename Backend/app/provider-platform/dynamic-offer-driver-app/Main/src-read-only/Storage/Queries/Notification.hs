{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Notification (module Storage.Queries.Notification, module ReExport) where

import qualified Domain.Types.Extra.Notification
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
import Storage.Queries.NotificationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Notification.Notification -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Notification.Notification] -> m ())
createMany = traverse_ create

findByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Notification.Notification))
findByShortId shortId = do findOneWithKV [Se.Is Beam.shortId $ Se.Eq shortId]

updateNotificationStatusAndResponseInfoById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Extra.Notification.NotificationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Notification.Notification -> m ())
updateNotificationStatusAndResponseInfoById status responseCode responseMessage id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseMessage responseMessage,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Notification.Notification -> m (Maybe Domain.Types.Notification.Notification))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Notification.Notification -> m ())
updateByPrimaryKey (Domain.Types.Notification.Notification {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.dateCreated dateCreated,
      Se.Set Beam.description description,
      Se.Set Beam.driverFeeId (Kernel.Types.Id.getId driverFeeId),
      Se.Set Beam.juspayProvidedId juspayProvidedId,
      Se.Set Beam.lastStatusCheckedAt lastStatusCheckedAt,
      Se.Set Beam.lastUpdated lastUpdated,
      Se.Set Beam.mandateId (Kernel.Types.Id.getId mandateId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Prelude.Just (Kernel.Types.Id.getId merchantOperatingCityId)),
      Se.Set Beam.notificationType notificationType,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.responseCode responseCode,
      Se.Set Beam.responseMessage responseMessage,
      Se.Set Beam.shortId shortId,
      Se.Set Beam.sourceAmount sourceAmount,
      Se.Set Beam.status status,
      Se.Set Beam.txnDate txnDate,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
