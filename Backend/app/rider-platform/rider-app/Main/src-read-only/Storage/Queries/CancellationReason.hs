{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CancellationReason (module Storage.Queries.CancellationReason, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.CancellationReasonExtra as ReExport
import Storage.Queries.Transformers.CancellationReason
import qualified Domain.Types.CancellationReason
import qualified Storage.Beam.CancellationReason as Beam
import qualified Domain.Types.Extra.CancellationReason
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationReason.CancellationReason] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Extra.CancellationReason.CancellationReasonCode -> m (Maybe Domain.Types.CancellationReason.CancellationReason))
findByPrimaryKey reasonCode = do findOneWithKV [Se.And [Se.Is Beam.reasonCode $ Se.Eq reasonCode]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
updateByPrimaryKey (Domain.Types.CancellationReason.CancellationReason {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.description description,
                                                                                                 Se.Set Beam.enabled enabled,
                                                                                                 Se.Set Beam.onAssign onAssign,
                                                                                                 Se.Set Beam.onConfirm onConfirm,
                                                                                                 Se.Set Beam.onInit onInit,
                                                                                                 Se.Set Beam.onSearch onSearch,
                                                                                                 Se.Set Beam.priority priority,
                                                                                                 Se.Set Beam.updatedAt (Just _now)] [Se.And [Se.Is Beam.reasonCode $ Se.Eq reasonCode]]}



