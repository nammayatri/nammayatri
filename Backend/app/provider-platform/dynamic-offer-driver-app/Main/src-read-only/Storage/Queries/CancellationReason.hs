{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.CancellationReason where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.CancellationReason
import qualified Domain.Types.CancellationReason
import qualified Storage.Beam.CancellationReason as Beam
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CancellationReason.CancellationReason] -> m ())
createMany = traverse_ create
findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Prelude.Bool -> m ([Domain.Types.CancellationReason.CancellationReason]))
findAll limit offset enabled = do findAllWithOptionsDb [Se.Is Beam.enabled $ Se.Eq enabled] (Se.Desc Beam.priority) limit offset
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReasonCode -> m (Maybe Domain.Types.CancellationReason.CancellationReason))
findByPrimaryKey reasonCode = do findOneWithKV [Se.And [Se.Is Beam.reasonCode $ Se.Eq (reasonCodeToText reasonCode)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CancellationReason.CancellationReason -> m ())
updateByPrimaryKey (Domain.Types.CancellationReason.CancellationReason {..}) = do {_now <- getCurrentTime;
                                                                                   updateWithKV [Se.Set Beam.description description,
                                                                                                 Se.Set Beam.enabled enabled,
                                                                                                 Se.Set Beam.priority priority,
                                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.reasonCode $ Se.Eq (reasonCodeToText reasonCode)]]}



instance FromTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason
    where fromTType' (Beam.CancellationReasonT {..}) = do pure $ Just Domain.Types.CancellationReason.CancellationReason{description = description,
                                                                                                                         enabled = enabled,
                                                                                                                         priority = priority,
                                                                                                                         reasonCode = reasonCode,
                                                                                                                         createdAt = createdAt,
                                                                                                                         updatedAt = updatedAt}
instance ToTType' Beam.CancellationReason Domain.Types.CancellationReason.CancellationReason
    where toTType' (Domain.Types.CancellationReason.CancellationReason {..}) = do Beam.CancellationReasonT{Beam.description = description,
                                                                                                           Beam.enabled = enabled,
                                                                                                           Beam.priority = priority,
                                                                                                           Beam.reasonCode = reasonCodeToText reasonCode,
                                                                                                           Beam.createdAt = createdAt,
                                                                                                           Beam.updatedAt = updatedAt}



