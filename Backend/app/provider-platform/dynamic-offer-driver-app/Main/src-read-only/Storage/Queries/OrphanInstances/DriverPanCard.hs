{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverPanCard where

import qualified Domain.Types.DriverPanCard
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverPanCard as Beam

instance FromTType' Beam.DriverPanCard Domain.Types.DriverPanCard.DriverPanCard where
  fromTType' (Beam.DriverPanCardT {..}) = do
    pure $
      Just
        Domain.Types.DriverPanCard.DriverPanCard
          { consent = consent,
            consentTimestamp = consentTimestamp,
            docType = docType,
            documentImageId1 = Kernel.Types.Id.Id documentImageId1,
            documentImageId2 = Kernel.Types.Id.Id <$> documentImageId2,
            driverDob = driverDob,
            driverId = Kernel.Types.Id.Id driverId,
            driverName = driverName,
            driverNameOnGovtDB = driverNameOnGovtDB,
            failedRules = failedRules,
            id = Kernel.Types.Id.Id id,
            isStrictlyVerified = isStrictlyVerified,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            panCardNumber = EncryptedHashed (Encrypted panCardNumberEncrypted) panCardNumberHash,
            verificationStatus = verificationStatus,
            verifiedBy = verifiedBy,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverPanCard Domain.Types.DriverPanCard.DriverPanCard where
  toTType' (Domain.Types.DriverPanCard.DriverPanCard {..}) = do
    Beam.DriverPanCardT
      { Beam.consent = consent,
        Beam.consentTimestamp = consentTimestamp,
        Beam.docType = docType,
        Beam.documentImageId1 = Kernel.Types.Id.getId documentImageId1,
        Beam.documentImageId2 = Kernel.Types.Id.getId <$> documentImageId2,
        Beam.driverDob = driverDob,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverName = driverName,
        Beam.driverNameOnGovtDB = driverNameOnGovtDB,
        Beam.failedRules = failedRules,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isStrictlyVerified = isStrictlyVerified,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.panCardNumberEncrypted = panCardNumber & unEncrypted . encrypted,
        Beam.panCardNumberHash = panCardNumber & hash,
        Beam.verificationStatus = verificationStatus,
        Beam.verifiedBy = verifiedBy,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
