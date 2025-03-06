{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Vehicle where

import qualified Domain.Types.Vehicle
import qualified Domain.Types.VehicleVariant
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Vehicle as Beam

instance FromTType' Beam.Vehicle Domain.Types.Vehicle.Vehicle where
  fromTType' (Beam.VehicleT {..}) = do
    pure $
      Just
        Domain.Types.Vehicle.Vehicle
          { airConditioned = airConditioned,
            capacity = capacity,
            category = Domain.Types.VehicleVariant.getVehicleCategory category variant,
            color = color,
            downgradeReason = downgradeReason,
            driverId = Kernel.Types.Id.Id driverId,
            energyType = energyType,
            luggageCapacity = luggageCapacity,
            mYManufacturing = mYManufacturing,
            make = make,
            merchantId = Kernel.Types.Id.Id merchantId,
            model = model,
            oxygen = oxygen,
            registrationCategory = registrationCategory,
            registrationNo = registrationNo,
            selectedServiceTiers = selectedServiceTiers,
            size = size,
            variant = variant,
            vehicleClass = vehicleClass,
            vehicleName = vehicleName,
            vehicleRating = vehicleRating,
            vehicleTags = vehicleTags,
            ventilator = ventilator,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Vehicle Domain.Types.Vehicle.Vehicle where
  toTType' (Domain.Types.Vehicle.Vehicle {..}) = do
    Beam.VehicleT
      { Beam.airConditioned = airConditioned,
        Beam.capacity = capacity,
        Beam.category = category,
        Beam.color = color,
        Beam.downgradeReason = downgradeReason,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.energyType = energyType,
        Beam.luggageCapacity = luggageCapacity,
        Beam.mYManufacturing = mYManufacturing,
        Beam.make = make,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.model = model,
        Beam.oxygen = oxygen,
        Beam.registrationCategory = registrationCategory,
        Beam.registrationNo = registrationNo,
        Beam.selectedServiceTiers = selectedServiceTiers,
        Beam.size = size,
        Beam.variant = variant,
        Beam.vehicleClass = vehicleClass,
        Beam.vehicleName = vehicleName,
        Beam.vehicleRating = vehicleRating,
        Beam.vehicleTags = vehicleTags,
        Beam.ventilator = ventilator,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
