{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleRegistrationCertificate where

import qualified Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleRegistrationCertificate as Beam

instance FromTType' Beam.VehicleRegistrationCertificate Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate where
  fromTType' (Beam.VehicleRegistrationCertificateT {..}) = do
    pure $
      Just
        Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate
          { airConditioned = airConditioned,
            approved = approved,
            certificateNumber = EncryptedHashed (Encrypted certificateNumberEncrypted) certificateNumberHash,
            dateOfRegistration = dateOfRegistration,
            documentImageId = Kernel.Types.Id.Id documentImageId,
            failedRules = failedRules,
            fitnessExpiry = fitnessExpiry,
            fleetOwnerId = fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            insuranceValidity = insuranceValidity,
            luggageCapacity = luggageCapacity,
            mYManufacturing = mYManufacturing,
            manufacturerModel = manufacturerModel,
            oxygen = oxygen,
            permitExpiry = permitExpiry,
            pucExpiry = pucExpiry,
            rejectReason = rejectReason,
            reviewRequired = reviewRequired,
            reviewedAt = reviewedAt,
            unencryptedCertificateNumber = unencryptedCertificateNumber,
            userPassedVehicleCategory = userPassedVehicleCategory,
            vehicleCapacity = vehicleCapacity,
            vehicleClass = vehicleClass,
            vehicleColor = vehicleColor,
            vehicleDoors = vehicleDoors,
            vehicleEnergyType = vehicleEnergyType,
            vehicleManufacturer = vehicleManufacturer,
            vehicleModel = vehicleModel,
            vehicleModelYear = vehicleModelYear,
            vehicleRating = vehicleRating,
            vehicleSeatBelts = vehicleSeatBelts,
            vehicleVariant = vehicleVariant,
            ventilator = ventilator,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleRegistrationCertificate Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate where
  toTType' (Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate {..}) = do
    Beam.VehicleRegistrationCertificateT
      { Beam.airConditioned = airConditioned,
        Beam.approved = approved,
        Beam.certificateNumberEncrypted = certificateNumber & unEncrypted . encrypted,
        Beam.certificateNumberHash = certificateNumber & hash,
        Beam.dateOfRegistration = dateOfRegistration,
        Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.failedRules = failedRules,
        Beam.fitnessExpiry = fitnessExpiry,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuranceValidity = insuranceValidity,
        Beam.luggageCapacity = luggageCapacity,
        Beam.mYManufacturing = mYManufacturing,
        Beam.manufacturerModel = manufacturerModel,
        Beam.oxygen = oxygen,
        Beam.permitExpiry = permitExpiry,
        Beam.pucExpiry = pucExpiry,
        Beam.rejectReason = rejectReason,
        Beam.reviewRequired = reviewRequired,
        Beam.reviewedAt = reviewedAt,
        Beam.unencryptedCertificateNumber = unencryptedCertificateNumber,
        Beam.userPassedVehicleCategory = userPassedVehicleCategory,
        Beam.vehicleCapacity = vehicleCapacity,
        Beam.vehicleClass = vehicleClass,
        Beam.vehicleColor = vehicleColor,
        Beam.vehicleDoors = vehicleDoors,
        Beam.vehicleEnergyType = vehicleEnergyType,
        Beam.vehicleManufacturer = vehicleManufacturer,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleModelYear = vehicleModelYear,
        Beam.vehicleRating = vehicleRating,
        Beam.vehicleSeatBelts = vehicleSeatBelts,
        Beam.vehicleVariant = vehicleVariant,
        Beam.ventilator = ventilator,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
