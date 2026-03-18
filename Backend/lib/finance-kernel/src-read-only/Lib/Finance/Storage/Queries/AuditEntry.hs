{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.AuditEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Finance.Audit
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Storage.Beam.AuditEntry as Beam
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.AuditEntry.AuditEntry -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.AuditEntry.AuditEntry] -> m ())
createMany = traverse_ create

findByAction :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Finance.Audit.AuditEntityType -> Kernel.Types.Finance.Audit.AuditAction -> m ([Lib.Finance.Domain.Types.AuditEntry.AuditEntry]))
findByAction entityType action = do findAllWithKV [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.action $ Se.Eq action]]

findByActor :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Finance.Audit.AuditActorType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.AuditEntry.AuditEntry]))
findByActor actorType actorId = do findAllWithKV [Se.And [Se.Is Beam.actorType $ Se.Eq actorType, Se.Is Beam.actorId $ Se.Eq actorId]]

findByEntity :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Finance.Audit.AuditEntityType -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.AuditEntry.AuditEntry]))
findByEntity entityType entityId = do findAllWithKV [Se.And [Se.Is Beam.entityType $ Se.Eq entityType, Se.Is Beam.entityId $ Se.Eq entityId]]

findById :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.AuditEntry.AuditEntry -> m (Maybe Lib.Finance.Domain.Types.AuditEntry.AuditEntry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.AuditEntry.AuditEntry -> m (Maybe Lib.Finance.Domain.Types.AuditEntry.AuditEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

-- | IMMUTABILITY GUARD: Audit entries must never be updated.
--   This function exists only because the code generator produces it.
--   Any call to this function will result in a runtime error.
--   See LAW 2: Immutability of History (append-only).
updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.AuditEntry.AuditEntry -> m ())
updateByPrimaryKey _ = error "AUDIT IMMUTABILITY VIOLATION: updateByPrimaryKey is forbidden on finance_audit_entry. Audit records are append-only."

instance FromTType' Beam.AuditEntry Lib.Finance.Domain.Types.AuditEntry.AuditEntry where
  fromTType' (Beam.AuditEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.AuditEntry.AuditEntry
          { action = action,
            actorId = actorId,
            actorType = actorType,
            createdAt = createdAt,
            entityId = entityId,
            entityType = entityType,
            hashChain = hashChain,
            id = Kernel.Types.Id.Id id,
            ipAddress = ipAddress,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            metadata = metadata,
            newState = newState,
            previousState = previousState
          }

instance ToTType' Beam.AuditEntry Lib.Finance.Domain.Types.AuditEntry.AuditEntry where
  toTType' (Lib.Finance.Domain.Types.AuditEntry.AuditEntry {..}) = do
    Beam.AuditEntryT
      { Beam.action = action,
        Beam.actorId = actorId,
        Beam.actorType = actorType,
        Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.hashChain = hashChain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ipAddress = ipAddress,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.metadata = metadata,
        Beam.newState = newState,
        Beam.previousState = previousState
      }
