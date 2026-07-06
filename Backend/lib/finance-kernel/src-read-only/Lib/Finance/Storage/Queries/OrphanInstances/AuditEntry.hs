{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.AuditEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Storage.Beam.AuditEntry as Beam

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
            id = Kernel.Types.Id.Id id,
            ipAddress = ipAddress,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            metadata = metadata,
            newState = newState,
            previousState = previousState,
            updatedAt = updatedAt
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
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ipAddress = ipAddress,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.metadata = metadata,
        Beam.newState = newState,
        Beam.previousState = previousState,
        Beam.updatedAt = updatedAt
      }
