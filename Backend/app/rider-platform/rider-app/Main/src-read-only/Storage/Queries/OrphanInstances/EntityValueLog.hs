{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.EntityValueLog where

import qualified Domain.Types.EntityValueLog
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.EntityValueLog as Beam

instance FromTType' Beam.EntityValueLog Domain.Types.EntityValueLog.EntityValueLog where
  fromTType' (Beam.EntityValueLogT {..}) = do
    pure $
      Just
        Domain.Types.EntityValueLog.EntityValueLog
          { actorId = actorId,
            actorType = actorType,
            delta = delta,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            operationId = operationId,
            operationType = operationType,
            reason = reason,
            status = status,
            valueAfter = valueAfter,
            valueBefore = valueBefore,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.EntityValueLog Domain.Types.EntityValueLog.EntityValueLog where
  toTType' (Domain.Types.EntityValueLog.EntityValueLog {..}) = do
    Beam.EntityValueLogT
      { Beam.actorId = actorId,
        Beam.actorType = actorType,
        Beam.delta = delta,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.operationId = operationId,
        Beam.operationType = operationType,
        Beam.reason = reason,
        Beam.status = status,
        Beam.valueAfter = valueAfter,
        Beam.valueBefore = valueBefore,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
