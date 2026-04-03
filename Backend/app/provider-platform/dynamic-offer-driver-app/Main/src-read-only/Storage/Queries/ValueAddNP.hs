{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.ValueAddNP where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.ValueAddNP
import qualified Storage.Beam.ValueAddNP as Beam
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ValueAddNP.ValueAddNP -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ValueAddNP.ValueAddNP] -> m ())
createMany = traverse_ create
findAll :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> m ([Domain.Types.ValueAddNP.ValueAddNP]))
findAll enabled = do findAllWithKV [Se.Is Beam.enabled $ Se.Eq enabled]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.ValueAddNP.ValueAddNP))
findByPrimaryKey subscriberId = do findOneWithKV [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ValueAddNP.ValueAddNP -> m ())
updateByPrimaryKey (Domain.Types.ValueAddNP.ValueAddNP {..}) = do {_now <- getCurrentTime;
                                                                   updateWithKV [Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.subscriberId $ Se.Eq subscriberId]]}



instance FromTType' Beam.ValueAddNP Domain.Types.ValueAddNP.ValueAddNP
    where fromTType' (Beam.ValueAddNPT {..}) = do pure $ Just Domain.Types.ValueAddNP.ValueAddNP{enabled = enabled, subscriberId = subscriberId, createdAt = createdAt, updatedAt = updatedAt}
instance ToTType' Beam.ValueAddNP Domain.Types.ValueAddNP.ValueAddNP
    where toTType' (Domain.Types.ValueAddNP.ValueAddNP {..}) = do Beam.ValueAddNPT{Beam.enabled = enabled, Beam.subscriberId = subscriberId, Beam.createdAt = createdAt, Beam.updatedAt = updatedAt}



