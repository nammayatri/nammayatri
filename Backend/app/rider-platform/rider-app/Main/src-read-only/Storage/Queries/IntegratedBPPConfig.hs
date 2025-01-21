{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfig where

import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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

findByDomainAndCityAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByPrimaryKey domain merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
updateByPrimaryKey (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.configJSON (Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

instance FromTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  fromTType' (Beam.IntegratedBPPConfigT {..}) = do
    providerConfig' <- Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfig configJSON
    pure $
      Just
        Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig
          { domain = domain,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            providerConfig = providerConfig',
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  toTType' (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
    Beam.IntegratedBPPConfigT
      { Beam.domain = domain,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.configJSON = Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
