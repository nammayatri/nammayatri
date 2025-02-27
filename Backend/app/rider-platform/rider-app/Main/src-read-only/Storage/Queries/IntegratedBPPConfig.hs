{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntegratedBPPConfig where

import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
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
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.OnDemand.Enums.VehicleCategory -> Domain.Types.IntegratedBPPConfig.PlatformType -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByDomainAndCityAndVehicleCategory domain merchantOperatingCityId vehicleCategory platformType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.platformType $ Se.Eq platformType
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> BecknV2.OnDemand.Enums.VehicleCategory -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByPrimaryKey domain id merchantId merchantOperatingCityId vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
updateByPrimaryKey (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.platformType platformType,
      Se.Set Beam.configJSON (Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
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
      { Beam.domain = domain,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.platformType = platformType,
        Beam.configJSON = Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
