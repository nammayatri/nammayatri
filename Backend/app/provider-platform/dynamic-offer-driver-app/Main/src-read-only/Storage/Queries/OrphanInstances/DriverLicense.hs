{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverLicense where

import qualified Domain.Types.DriverLicense
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverLicense as Beam

instance FromTType' Beam.DriverLicense Domain.Types.DriverLicense.DriverLicense where
  fromTType' (Beam.DriverLicenseT {..}) = do
    pure $
      Just
        Domain.Types.DriverLicense.DriverLicense
          { classOfVehicles = classOfVehicles,
            consent = consent,
            consentTimestamp = consentTimestamp,
            dateOfIssue = dateOfIssue,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            driverDob = driverDob,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            failedRules = failedRules,
            id = Kernel.Types.Id.Id id,
            licenseExpiry = licenseExpiry,
            licenseNumber = EncryptedHashed (Encrypted licenseNumberEncrypted) licenseNumberHash,
            rejectReason = rejectReason,
            vehicleCategory = vehicleCategory,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverLicense Domain.Types.DriverLicense.DriverLicense where
  toTType' (Domain.Types.DriverLicense.DriverLicense {..}) = do
    Beam.DriverLicenseT
      { Beam.classOfVehicles = classOfVehicles,
        Beam.consent = consent,
        Beam.consentTimestamp = consentTimestamp,
        Beam.dateOfIssue = dateOfIssue,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.driverDob = driverDob,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.failedRules = failedRules,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.licenseExpiry = licenseExpiry,
        Beam.licenseNumberEncrypted = licenseNumber & unEncrypted . encrypted,
        Beam.licenseNumberHash = licenseNumber & hash,
        Beam.rejectReason = rejectReason,
        Beam.vehicleCategory = vehicleCategory,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
