{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfig where

import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IntegratedBPPConfig as Beam
import qualified Storage.Queries.Transformers.IntegratedBPPConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig] -> m ())
createMany = traverse_ create

findAllByPlatformAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.IntegratedBPPConfig.PlatformType -> m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig])
findAllByPlatformAndVehicleCategory domain vehicleCategory platformType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.platformType $ Se.Eq platformType
        ]
    ]

findByAgencyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByAgencyId agencyKey = do findOneWithKV [Se.And [Se.Is Beam.agencyKey $ Se.Eq agencyKey]]

findByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.IntegratedBPPConfig.PlatformType -> m [Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig])
findByDomainAndCityAndVehicleCategory domain city vehicleCategory platformType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.city $ Se.Eq city,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.platformType $ Se.Eq platformType
        ]
    ]

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
      Se.Set Beam.city city,
      Se.Set Beam.domain domain,
      Se.Set Beam.feedKey feedKey,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.platformType platformType,
      Se.Set Beam.configJSON (Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig),
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  fromTType' (Beam.IntegratedBPPConfigT {..}) = do
    providerConfig' <- Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfig configJSON
    pure $
      Just
        Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig
          { agencyKey = agencyKey,
            city = city,
            domain = domain,
            feedKey = feedKey,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            platformType = platformType,
            providerConfig = providerConfig',
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  toTType' (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
    Beam.IntegratedBPPConfigT
      { Beam.agencyKey = agencyKey,
        Beam.city = city,
        Beam.domain = domain,
        Beam.feedKey = feedKey,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.platformType = platformType,
        Beam.configJSON = Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
