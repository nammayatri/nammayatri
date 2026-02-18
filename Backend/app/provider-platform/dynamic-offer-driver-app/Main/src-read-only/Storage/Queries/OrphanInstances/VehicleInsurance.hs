{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VehicleInsurance where

import qualified Domain.Types.VehicleInsurance
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VehicleInsurance as Beam

instance FromTType' Beam.VehicleInsurance Domain.Types.VehicleInsurance.VehicleInsurance where
  fromTType' (Beam.VehicleInsuranceT {..}) = do
    pure $
      Just
        Domain.Types.VehicleInsurance.VehicleInsurance
          { documentImageId = Kernel.Types.Id.Id documentImageId,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            insuredName = insuredName,
            issueDate = issueDate,
            limitsOfLiability = limitsOfLiability,
            policyExpiry = policyExpiry,
            policyNumber = EncryptedHashed (Encrypted policyNumberEncrypted) policyNumberHash,
            policyProvider = policyProvider,
            rcId = Kernel.Types.Id.Id rcId,
            rejectReason = rejectReason,
            verificationStatus = verificationStatus,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleInsurance Domain.Types.VehicleInsurance.VehicleInsurance where
  toTType' (Domain.Types.VehicleInsurance.VehicleInsurance {..}) = do
    Beam.VehicleInsuranceT
      { Beam.documentImageId = Kernel.Types.Id.getId documentImageId,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.insuredName = insuredName,
        Beam.issueDate = issueDate,
        Beam.limitsOfLiability = limitsOfLiability,
        Beam.policyExpiry = policyExpiry,
        Beam.policyNumberEncrypted = ((policyNumber & unEncrypted . encrypted)),
        Beam.policyNumberHash = (policyNumber & hash),
        Beam.policyProvider = policyProvider,
        Beam.rcId = Kernel.Types.Id.getId rcId,
        Beam.rejectReason = rejectReason,
        Beam.verificationStatus = verificationStatus,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
