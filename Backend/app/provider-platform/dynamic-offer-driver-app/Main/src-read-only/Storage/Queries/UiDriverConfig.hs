{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.UiDriverConfig (module Storage.Queries.UiDriverConfig, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.UiDriverConfigExtra as ReExport
import qualified Domain.Types.UiDriverConfig
import qualified Storage.Beam.UiDriverConfig as Beam
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.UiDriverConfig.UiDriverConfig -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.UiDriverConfig.UiDriverConfig] -> m ())
createMany = traverse_ create
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.UiDriverConfig.UiDriverConfig -> m (Maybe Domain.Types.UiDriverConfig.UiDriverConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.UiDriverConfig.UiDriverConfig -> m ())
updateByPrimaryKey (Domain.Types.UiDriverConfig.UiDriverConfig {..}) = do {_now <- getCurrentTime;
                                                                           updateWithKV [Se.Set Beam.config config,
                                                                                         Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                         Se.Set Beam.os os,
                                                                                         Se.Set Beam.platform platform,
                                                                                         Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



