{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverGstin where

import qualified Domain.Types.DriverGstin
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverGstin as Beam

instance FromTType' Beam.DriverGstin Domain.Types.DriverGstin.DriverGstin where
  fromTType' (Beam.DriverGstinT {..}) = do
    pure $
      Just
        Domain.Types.DriverGstin.DriverGstin
          { address = address,
            constitutionOfBusiness = constitutionOfBusiness,
            dateOfLiability = dateOfLiability,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            gstin = EncryptedHashed (Encrypted gstinEncrypted) gstinHash,
            id = Kernel.Types.Id.Id id,
            isProvisional = isProvisional,
            legalName = legalName,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            panNumber = panNumber,
            tradeName = tradeName,
            typeOfRegistration = typeOfRegistration,
            validFrom = validFrom,
            validUpto = validUpto,
            verificationStatus = verificationStatus,
            verifiedBy = verifiedBy,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGstin Domain.Types.DriverGstin.DriverGstin where
  toTType' (Domain.Types.DriverGstin.DriverGstin {..}) = do
    Beam.DriverGstinT
      { Beam.address = address,
        Beam.constitutionOfBusiness = constitutionOfBusiness,
        Beam.dateOfLiability = dateOfLiability,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.gstinEncrypted = ((gstin & unEncrypted . encrypted)),
        Beam.gstinHash = (gstin & hash),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isProvisional = isProvisional,
        Beam.legalName = legalName,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.panNumber = panNumber,
        Beam.tradeName = tradeName,
        Beam.typeOfRegistration = typeOfRegistration,
        Beam.validFrom = validFrom,
        Beam.validUpto = validUpto,
        Beam.verificationStatus = verificationStatus,
        Beam.verifiedBy = verifiedBy,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
