{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CancellationReason where

import qualified Domain.Types.CancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CancellationReason as Beam
import Storage.Queries.Transformers.CancellationReason

instance FromTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason where
  fromTType' (Beam.CancellationReasonT {..}) = do
    createdAt' <- getCreatedAt createdAt
    updatedAt' <- getUpdatedAt updatedAt
    pure $
      Just
        Domain.Types.CancellationReason.CancellationReason
          { reasonCode = reasonCode,
            description = description,
            enabled = enabled,
            onSearch = onSearch,
            onInit = onInit,
            onConfirm = onConfirm,
            onAssign = onAssign,
            priority = priority,
            createdAt = createdAt',
            updatedAt = updatedAt'
          }

instance ToTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason where
  toTType' (Domain.Types.CancellationReason.CancellationReason {..}) = do
    Beam.CancellationReasonT
      { Beam.reasonCode = reasonCode,
        Beam.description = description,
        Beam.enabled = enabled,
        Beam.onSearch = onSearch,
        Beam.onInit = onInit,
        Beam.onConfirm = onConfirm,
        Beam.onAssign = onAssign,
        Beam.priority = priority,
        Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
