{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.InsuranceConfig where

import qualified Domain.Types.Common
import qualified Domain.Types.InsuranceConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.InsuranceConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.InsuranceConfig.InsuranceConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.InsuranceConfig.InsuranceConfig] -> m ())
createMany = traverse_ create

findByMerchantIdAndMerchantOperatingCityIdAndTripCategoryAndVehicleCategory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Common.TripCategory -> Domain.Types.VehicleCategory.VehicleCategory -> m (Maybe Domain.Types.InsuranceConfig.InsuranceConfig))
findByMerchantIdAndMerchantOperatingCityIdAndTripCategoryAndVehicleCategory merchantId merchantOperatingCityId tripCategory vehicleCategory = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.InsuranceConfig.InsuranceConfig -> m (Maybe Domain.Types.InsuranceConfig.InsuranceConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.InsuranceConfig.InsuranceConfig -> m ())
updateByPrimaryKey (Domain.Types.InsuranceConfig.InsuranceConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowedVehicleServiceTiers allowedVehicleServiceTiers,
      Se.Set Beam.city city,
      Se.Set Beam.driverInsuredAmount driverInsuredAmount,
      Se.Set Beam.hours hours,
      Se.Set Beam.insuredAmount insuredAmount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerId partnerId,
      Se.Set Beam.plan plan,
      Se.Set Beam.planType planType,
      Se.Set Beam.state state,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.InsuranceConfig Domain.Types.InsuranceConfig.InsuranceConfig where
  fromTType' (Beam.InsuranceConfigT {..}) = do
    pure $
      Just
        Domain.Types.InsuranceConfig.InsuranceConfig
          { allowedVehicleServiceTiers = allowedVehicleServiceTiers,
            city = city,
            createdAt = createdAt,
            driverInsuredAmount = driverInsuredAmount,
            hours = hours,
            id = Kernel.Types.Id.Id id,
            insuredAmount = insuredAmount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerId = partnerId,
            plan = plan,
            planType = planType,
            state = state,
            tripCategory = tripCategory,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.InsuranceConfig Domain.Types.InsuranceConfig.InsuranceConfig where
  toTType' (Domain.Types.InsuranceConfig.InsuranceConfig {..}) = do
    Beam.InsuranceConfigT
      { Beam.allowedVehicleServiceTiers = allowedVehicleServiceTiers,
        Beam.city = city,
        Beam.createdAt = createdAt,
        Beam.driverInsuredAmount = driverInsuredAmount,
        Beam.hours = hours,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuredAmount = insuredAmount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerId = partnerId,
        Beam.plan = plan,
        Beam.planType = planType,
        Beam.state = state,
        Beam.tripCategory = tripCategory,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
