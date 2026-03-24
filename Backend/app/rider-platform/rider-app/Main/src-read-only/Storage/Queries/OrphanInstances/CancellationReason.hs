{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.CancellationReason where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.CancellationReason
import qualified Domain.Types.CancellationReason
import qualified Storage.Beam.CancellationReason as Beam
import qualified Kernel.Prelude



instance FromTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason
    where fromTType' (Beam.CancellationReasonT {..}) = do {createdAt' <- getCreatedAt createdAt;
                                                           updatedAt' <- getUpdatedAt updatedAt;
                                                           pure $ Just Domain.Types.CancellationReason.CancellationReason{createdAt = createdAt',
                                                                                                                          description = description,
                                                                                                                          enabled = enabled,
                                                                                                                          onAssign = onAssign,
                                                                                                                          onConfirm = onConfirm,
                                                                                                                          onInit = onInit,
                                                                                                                          onSearch = onSearch,
                                                                                                                          priority = priority,
                                                                                                                          reasonCode = reasonCode,
                                                                                                                          updatedAt = updatedAt'}}
instance ToTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason
    where toTType' (Domain.Types.CancellationReason.CancellationReason {..}) = do Beam.CancellationReasonT{Beam.createdAt = Kernel.Prelude.Just createdAt,
                                                                                                           Beam.description = description,
                                                                                                           Beam.enabled = enabled,
                                                                                                           Beam.onAssign = onAssign,
                                                                                                           Beam.onConfirm = onConfirm,
                                                                                                           Beam.onInit = onInit,
                                                                                                           Beam.onSearch = onSearch,
                                                                                                           Beam.priority = priority,
                                                                                                           Beam.reasonCode = reasonCode,
                                                                                                           Beam.updatedAt = Kernel.Prelude.Just updatedAt}



