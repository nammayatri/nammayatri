{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentAuditLog where

import qualified Domain.Types.DocumentAuditLog
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DocumentAuditLog as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentAuditLog.DocumentAuditLog -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DocumentAuditLog.DocumentAuditLog] -> m ())
createMany = traverse_ create

findByEntity :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.DocumentAuditLog.DocumentAuditLog])
findByEntity limit offset entityId = do findAllWithOptionsKV [Se.Is Beam.entityId $ Se.Eq entityId] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DocumentAuditLog.DocumentAuditLog -> m (Maybe Domain.Types.DocumentAuditLog.DocumentAuditLog))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DocumentAuditLog.DocumentAuditLog -> m ())
updateByPrimaryKey (Domain.Types.DocumentAuditLog.DocumentAuditLog {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.action action,
      Se.Set Beam.actorId actorId,
      Se.Set Beam.actorRole actorRole,
      Se.Set Beam.actorSource actorSource,
      Se.Set Beam.details details,
      Se.Set Beam.documentRefId documentRefId,
      Se.Set Beam.documentRefType documentRefType,
      Se.Set Beam.documentType documentType,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.eventId eventId,
      Se.Set Beam.fieldName fieldName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.newStatus newStatus,
      Se.Set Beam.previousStatus previousStatus,
      Se.Set Beam.reason reason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DocumentAuditLog Domain.Types.DocumentAuditLog.DocumentAuditLog where
  fromTType' (Beam.DocumentAuditLogT {..}) = do
    pure $
      Just
        Domain.Types.DocumentAuditLog.DocumentAuditLog
          { action = action,
            actorId = actorId,
            actorRole = actorRole,
            actorSource = actorSource,
            createdAt = createdAt,
            details = details,
            documentRefId = documentRefId,
            documentRefType = documentRefType,
            documentType = documentType,
            entityId = entityId,
            entityType = entityType,
            eventId = eventId,
            fieldName = fieldName,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            newStatus = newStatus,
            previousStatus = previousStatus,
            reason = reason,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DocumentAuditLog Domain.Types.DocumentAuditLog.DocumentAuditLog where
  toTType' (Domain.Types.DocumentAuditLog.DocumentAuditLog {..}) = do
    Beam.DocumentAuditLogT
      { Beam.action = action,
        Beam.actorId = actorId,
        Beam.actorRole = actorRole,
        Beam.actorSource = actorSource,
        Beam.createdAt = createdAt,
        Beam.details = details,
        Beam.documentRefId = documentRefId,
        Beam.documentRefType = documentRefType,
        Beam.documentType = documentType,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.eventId = eventId,
        Beam.fieldName = fieldName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.newStatus = newStatus,
        Beam.previousStatus = previousStatus,
        Beam.reason = reason,
        Beam.updatedAt = updatedAt
      }
