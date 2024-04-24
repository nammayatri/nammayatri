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
          { createdAt = createdAt',
            description = description,
            enabled = enabled,
            onAssign = onAssign,
            onConfirm = onConfirm,
            onInit = onInit,
            onSearch = onSearch,
            priority = priority,
            reasonCode = reasonCode,
            updatedAt = updatedAt'
          }

instance ToTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason where
  toTType' (Domain.Types.CancellationReason.CancellationReason {..}) = do
    Beam.CancellationReasonT
      { Beam.createdAt = Kernel.Prelude.Just createdAt,
        Beam.description = description,
        Beam.enabled = enabled,
        Beam.onAssign = onAssign,
        Beam.onConfirm = onConfirm,
        Beam.onInit = onInit,
        Beam.onSearch = onSearch,
        Beam.priority = priority,
        Beam.reasonCode = reasonCode,
        Beam.updatedAt = Kernel.Prelude.Just updatedAt
      }
