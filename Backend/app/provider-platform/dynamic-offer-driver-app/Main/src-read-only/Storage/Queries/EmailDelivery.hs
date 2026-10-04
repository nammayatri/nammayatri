{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.EmailDelivery where

import qualified Domain.Types.EmailDelivery
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.EmailDelivery as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EmailDelivery.EmailDelivery -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.EmailDelivery.EmailDelivery] -> m ())
createMany = traverse_ create

findAllByOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.EmailDelivery.EmailDelivery]))
findAllByOwnerId ownerId = do findAllWithKV [Se.Is Beam.ownerId $ Se.Eq ownerId]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery -> m (Maybe Domain.Types.EmailDelivery.EmailDelivery))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByProviderMessageId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.EmailDelivery.EmailDelivery))
findByProviderMessageId providerMessageId = do findOneWithKV [Se.Is Beam.providerMessageId $ Se.Eq providerMessageId]

updateFailed ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.EmailDelivery.EmailDeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery -> m ())
updateFailed status failureReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.failureReason failureReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateProviderMessage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.EmailDelivery.EmailDeliveryProvider -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery -> m ())
updateProviderMessage provider providerMessageId sentAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.provider provider,
      Se.Set Beam.providerMessageId providerMessageId,
      Se.Set Beam.sentAt sentAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateSent ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.EmailDelivery.EmailDeliveryStatus -> Kernel.Prelude.Maybe Domain.Types.EmailDelivery.EmailDeliveryProvider -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery -> m ())
updateSent status provider providerMessageId sentAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.provider provider,
      Se.Set Beam.providerMessageId providerMessageId,
      Se.Set Beam.sentAt sentAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.EmailDelivery.EmailDelivery -> m (Maybe Domain.Types.EmailDelivery.EmailDelivery))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.EmailDelivery.EmailDelivery -> m ())
updateByPrimaryKey (Domain.Types.EmailDelivery.EmailDelivery {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bounceSubType bounceSubType,
      Se.Set Beam.bounceType bounceType,
      Se.Set Beam.deliveredAt deliveredAt,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.lastEventAt lastEventAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.ownerId ownerId,
      Se.Set Beam.ownerType ownerType,
      Se.Set Beam.provider provider,
      Se.Set Beam.providerMessageId providerMessageId,
      Se.Set Beam.sentAt sentAt,
      Se.Set Beam.status status,
      Se.Set Beam.toAddress toAddress,
      Se.Set Beam.triggeredBy triggeredBy,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.EmailDelivery Domain.Types.EmailDelivery.EmailDelivery where
  fromTType' (Beam.EmailDeliveryT {..}) = do
    pure $
      Just
        Domain.Types.EmailDelivery.EmailDelivery
          { bounceSubType = bounceSubType,
            bounceType = bounceType,
            createdAt = createdAt,
            deliveredAt = deliveredAt,
            failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            lastEventAt = lastEventAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            ownerId = ownerId,
            ownerType = ownerType,
            provider = provider,
            providerMessageId = providerMessageId,
            sentAt = sentAt,
            status = status,
            toAddress = toAddress,
            triggeredBy = triggeredBy,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EmailDelivery Domain.Types.EmailDelivery.EmailDelivery where
  toTType' (Domain.Types.EmailDelivery.EmailDelivery {..}) = do
    Beam.EmailDeliveryT
      { Beam.bounceSubType = bounceSubType,
        Beam.bounceType = bounceType,
        Beam.createdAt = createdAt,
        Beam.deliveredAt = deliveredAt,
        Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastEventAt = lastEventAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.ownerId = ownerId,
        Beam.ownerType = ownerType,
        Beam.provider = provider,
        Beam.providerMessageId = providerMessageId,
        Beam.sentAt = sentAt,
        Beam.status = status,
        Beam.toAddress = toAddress,
        Beam.triggeredBy = triggeredBy,
        Beam.updatedAt = updatedAt
      }
