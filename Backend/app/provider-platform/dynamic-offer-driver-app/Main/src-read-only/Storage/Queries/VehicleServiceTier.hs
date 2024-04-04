{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleServiceTier where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleServiceTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleServiceTier as Beam

create :: KvDbFlow m r => (Domain.Types.VehicleServiceTier.VehicleServiceTier -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.VehicleServiceTier.VehicleServiceTier] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.VehicleServiceTier.VehicleServiceTier])
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByServiceTierTypeAndCityId ::
  KvDbFlow m r =>
  (Domain.Types.ServiceTierType.ServiceTierType -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier))
findByServiceTierTypeAndCityId serviceTierType (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.serviceTierType $ Se.Eq serviceTierType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.VehicleServiceTier.VehicleServiceTier -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.VehicleServiceTier.VehicleServiceTier -> m ())
updateByPrimaryKey (Domain.Types.VehicleServiceTier.VehicleServiceTier {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditioned airConditioned,
      Se.Set Beam.allowedVehicleVariant allowedVehicleVariant,
      Se.Set Beam.autoSelectedVehicleVariant autoSelectedVehicleVariant,
      Se.Set Beam.defaultForVehicleVariant defaultForVehicleVariant,
      Se.Set Beam.driverRating driverRating,
      Se.Set Beam.longDescription longDescription,
      Se.Set Beam.luggageCapacity luggageCapacity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.priority priority,
      Se.Set Beam.seatingCapacity seatingCapacity,
      Se.Set Beam.serviceTierType serviceTierType,
      Se.Set Beam.shortDescription shortDescription,
      Se.Set Beam.vehicleRating vehicleRating,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleServiceTier Domain.Types.VehicleServiceTier.VehicleServiceTier where
  fromTType' (Beam.VehicleServiceTierT {..}) = do
    pure $
      Just
        Domain.Types.VehicleServiceTier.VehicleServiceTier
          { airConditioned = airConditioned,
            allowedVehicleVariant = allowedVehicleVariant,
            autoSelectedVehicleVariant = autoSelectedVehicleVariant,
            defaultForVehicleVariant = defaultForVehicleVariant,
            driverRating = driverRating,
            id = Kernel.Types.Id.Id id,
            longDescription = longDescription,
            luggageCapacity = luggageCapacity,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            priority = priority,
            seatingCapacity = seatingCapacity,
            serviceTierType = serviceTierType,
            shortDescription = shortDescription,
            vehicleRating = vehicleRating,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleServiceTier Domain.Types.VehicleServiceTier.VehicleServiceTier where
  toTType' (Domain.Types.VehicleServiceTier.VehicleServiceTier {..}) = do
    Beam.VehicleServiceTierT
      { Beam.airConditioned = airConditioned,
        Beam.allowedVehicleVariant = allowedVehicleVariant,
        Beam.autoSelectedVehicleVariant = autoSelectedVehicleVariant,
        Beam.defaultForVehicleVariant = defaultForVehicleVariant,
        Beam.driverRating = driverRating,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.longDescription = longDescription,
        Beam.luggageCapacity = luggageCapacity,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.priority = priority,
        Beam.seatingCapacity = seatingCapacity,
        Beam.serviceTierType = serviceTierType,
        Beam.shortDescription = shortDescription,
        Beam.vehicleRating = vehicleRating,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
