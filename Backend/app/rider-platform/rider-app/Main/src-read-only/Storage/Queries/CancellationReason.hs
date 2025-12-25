{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationReason (module Storage.Queries.CancellationReason, module ReExport) where

import qualified Domain.Types.CancellationReason
import qualified Domain.Types.Extra.CancellationReason
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as Beam
import Storage.Queries.CancellationReasonExtra as ReExport
import Storage.Queries.Transformers.CancellationReason

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationReason.CancellationReason] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Extra.CancellationReason.CancellationReasonCode -> m (Maybe Domain.Types.CancellationReason.CancellationReason))
findByPrimaryKey reasonCode = do findOneWithKV [Se.And [Se.Is Beam.reasonCode $ Se.Eq reasonCode]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
updateByPrimaryKey (Domain.Types.CancellationReason.CancellationReason {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.onAssign onAssign,
      Se.Set Beam.onConfirm onConfirm,
      Se.Set Beam.onInit onInit,
      Se.Set Beam.onSearch onSearch,
      Se.Set Beam.priority priority,
      Se.Set Beam.updatedAt (Just _now)
    ]
    [Se.And [Se.Is Beam.reasonCode $ Se.Eq reasonCode]]
