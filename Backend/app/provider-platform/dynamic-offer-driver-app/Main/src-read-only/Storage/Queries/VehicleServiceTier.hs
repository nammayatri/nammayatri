{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleServiceTier where

import qualified Domain.Types.Common
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleServiceTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleServiceTier as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleServiceTier.VehicleServiceTier -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleServiceTier.VehicleServiceTier] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.VehicleServiceTier.VehicleServiceTier])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findBaseServiceTierTypeByCategoryAndCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier))
findBaseServiceTierTypeByCategoryAndCityId vehicleCategory merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.baseVehicleServiceTier $ Se.Eq (Just True)
        ]
    ]

findByServiceTierTypeAndCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Common.ServiceTierType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier))
findByServiceTierTypeAndCityId serviceTierType merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.serviceTierType $ Se.Eq serviceTierType,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VehicleServiceTier.VehicleServiceTier -> m (Maybe Domain.Types.VehicleServiceTier.VehicleServiceTier))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleServiceTier.VehicleServiceTier -> m ())
updateByPrimaryKey (Domain.Types.VehicleServiceTier.VehicleServiceTier {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.airConditionedThreshold airConditionedThreshold,
      Se.Set Beam.allowedVehicleVariant allowedVehicleVariant,
      Se.Set Beam.autoSelectedVehicleVariant autoSelectedVehicleVariant,
      Se.Set Beam.baseVehicleServiceTier baseVehicleServiceTier,
      Se.Set Beam.defaultForVehicleVariant defaultForVehicleVariant,
      Se.Set Beam.driverRating driverRating,
      Se.Set Beam.fareAdditionPerKmOverBaseServiceTier fareAdditionPerKmOverBaseServiceTier,
      Se.Set Beam.isAirConditioned isAirConditioned,
      Se.Set Beam.isIntercityEnabled isIntercityEnabled,
      Se.Set Beam.isRentalsEnabled isRentalsEnabled,
      Se.Set Beam.longDescription longDescription,
      Se.Set Beam.luggageCapacity luggageCapacity,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.oxygen oxygen,
      Se.Set Beam.priority priority,
      Se.Set Beam.scheduleBookingListEligibilityTags scheduleBookingListEligibilityTags,
      Se.Set Beam.seatingCapacity seatingCapacity,
      Se.Set Beam.serviceTierType serviceTierType,
      Se.Set Beam.shortDescription shortDescription,
      Se.Set Beam.stopFcmSuppressCount stopFcmSuppressCount,
      Se.Set Beam.stopFcmThreshold stopFcmThreshold,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.vehicleIconUrl (Kernel.Prelude.fmap showBaseUrl vehicleIconUrl),
      Se.Set Beam.vehicleRating vehicleRating,
      Se.Set Beam.ventilator ventilator,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleServiceTier Domain.Types.VehicleServiceTier.VehicleServiceTier where
  fromTType' (Beam.VehicleServiceTierT {..}) = do
    vehicleIconUrl' <- Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl) vehicleIconUrl
    pure $
      Just
        Domain.Types.VehicleServiceTier.VehicleServiceTier
          { airConditionedThreshold = airConditionedThreshold,
            allowedVehicleVariant = allowedVehicleVariant,
            autoSelectedVehicleVariant = autoSelectedVehicleVariant,
            baseVehicleServiceTier = baseVehicleServiceTier,
            defaultForVehicleVariant = defaultForVehicleVariant,
            driverRating = driverRating,
            fareAdditionPerKmOverBaseServiceTier = fareAdditionPerKmOverBaseServiceTier,
            id = Kernel.Types.Id.Id id,
            isAirConditioned = isAirConditioned,
            isIntercityEnabled = isIntercityEnabled,
            isRentalsEnabled = isRentalsEnabled,
            longDescription = longDescription,
            luggageCapacity = luggageCapacity,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            oxygen = oxygen,
            priority = priority,
            scheduleBookingListEligibilityTags = scheduleBookingListEligibilityTags,
            seatingCapacity = seatingCapacity,
            serviceTierType = serviceTierType,
            shortDescription = shortDescription,
            stopFcmSuppressCount = stopFcmSuppressCount,
            stopFcmThreshold = stopFcmThreshold,
            vehicleCategory = vehicleCategory,
            vehicleIconUrl = vehicleIconUrl',
            vehicleRating = vehicleRating,
            ventilator = ventilator,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleServiceTier Domain.Types.VehicleServiceTier.VehicleServiceTier where
  toTType' (Domain.Types.VehicleServiceTier.VehicleServiceTier {..}) = do
    Beam.VehicleServiceTierT
      { Beam.airConditionedThreshold = airConditionedThreshold,
        Beam.allowedVehicleVariant = allowedVehicleVariant,
        Beam.autoSelectedVehicleVariant = autoSelectedVehicleVariant,
        Beam.baseVehicleServiceTier = baseVehicleServiceTier,
        Beam.defaultForVehicleVariant = defaultForVehicleVariant,
        Beam.driverRating = driverRating,
        Beam.fareAdditionPerKmOverBaseServiceTier = fareAdditionPerKmOverBaseServiceTier,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAirConditioned = isAirConditioned,
        Beam.isIntercityEnabled = isIntercityEnabled,
        Beam.isRentalsEnabled = isRentalsEnabled,
        Beam.longDescription = longDescription,
        Beam.luggageCapacity = luggageCapacity,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.oxygen = oxygen,
        Beam.priority = priority,
        Beam.scheduleBookingListEligibilityTags = scheduleBookingListEligibilityTags,
        Beam.seatingCapacity = seatingCapacity,
        Beam.serviceTierType = serviceTierType,
        Beam.shortDescription = shortDescription,
        Beam.stopFcmSuppressCount = stopFcmSuppressCount,
        Beam.stopFcmThreshold = stopFcmThreshold,
        Beam.vehicleCategory = vehicleCategory,
        Beam.vehicleIconUrl = Kernel.Prelude.fmap showBaseUrl vehicleIconUrl,
        Beam.vehicleRating = vehicleRating,
        Beam.ventilator = ventilator,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
