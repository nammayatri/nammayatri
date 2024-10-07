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
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
updateByPrimaryKey (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.domain domain,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.qrGeneratedBy qrGeneratedBy,
      Se.Set Beam.qrGenerationKey qrGenerationKey,
      Se.Set Beam.qrVerificationKey qrVerificationKey,
      Se.Set Beam.qrVerifiedBy qrVerifiedBy,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  fromTType' (Beam.IntegratedBPPConfigT {..}) = do
    pure $
      Just
        Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig
          { domain = domain,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            qrGeneratedBy = qrGeneratedBy,
            qrGenerationKey = qrGenerationKey,
            qrVerificationKey = qrVerificationKey,
            qrVerifiedBy = qrVerifiedBy,
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
        Beam.qrGeneratedBy = qrGeneratedBy,
        Beam.qrGenerationKey = qrGenerationKey,
        Beam.qrVerificationKey = qrVerificationKey,
        Beam.qrVerifiedBy = qrVerifiedBy,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
