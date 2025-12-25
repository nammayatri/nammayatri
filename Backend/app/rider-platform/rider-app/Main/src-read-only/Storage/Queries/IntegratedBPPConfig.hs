{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfig (module Storage.Queries.IntegratedBPPConfig, module ReExport) where

import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IntegratedBPPConfig as Beam
import Storage.Queries.IntegratedBPPConfigExtra as ReExport
import qualified Storage.Queries.Transformers.IntegratedBPPConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig] -> m ())
createMany = traverse_ create

findByAgencyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByAgencyId agencyKey = do findOneWithKV [Se.And [Se.Is Beam.agencyKey $ Se.Eq agencyKey]]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
updateByPrimaryKey (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agencyKey agencyKey,
      Se.Set Beam.domain domain,
      Se.Set Beam.feedKey feedKey,
      Se.Set Beam.isTicketValidOnMultipleRoutes isTicketValidOnMultipleRoutes,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.platformType platformType,
      Se.Set Beam.configJSON (Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig),
      Se.Set Beam.providerName providerName,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
