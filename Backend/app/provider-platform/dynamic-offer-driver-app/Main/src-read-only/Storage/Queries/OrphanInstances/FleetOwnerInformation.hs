{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetOwnerInformation where

import qualified Domain.Types.FleetOwnerInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetOwnerInformation as Beam
import qualified Storage.Queries.Transformers.FleetOwnerInformation

instance FromTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  fromTType' (Beam.FleetOwnerInformationT {..}) = do
    pure $
      Just
        Domain.Types.FleetOwnerInformation.FleetOwnerInformation
          { aadhaarBackImageId = aadhaarBackImageId,
            aadhaarFrontImageId = aadhaarFrontImageId,
            aadhaarNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem aadhaarNumberEncrypted aadhaarNumberHash,
            aadhaarNumberDec = aadhaarNumber,
            blocked = blocked,
            businessLicenseImageId = businessLicenseImageId,
            businessLicenseNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem businessLicenseNumberEncrypted businessLicenseNumberHash,
            businessLicenseNumberDec = businessLicenseNumber,
            enabled = enabled,
            fleetOwnerPersonId = Kernel.Types.Id.Id fleetOwnerPersonId,
            fleetType = fleetType,
            gstImageId = gstImageId,
            gstNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem gstNumberEncrypted gstNumberHash,
            gstNumberDec = gstNumber,
            isEligibleForSubscription = fromMaybe True isEligibleForSubscription,
            merchantId = Kernel.Types.Id.Id merchantId,
            panImageId = panImageId,
            panNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem panNumberEncrypted panNumberHash,
            panNumberDec = panNumber,
            referredByOperatorId = referredByOperatorId,
            registeredAt = registeredAt,
            verified = verified,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  toTType' (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
    Beam.FleetOwnerInformationT
      { Beam.aadhaarBackImageId = aadhaarBackImageId,
        Beam.aadhaarFrontImageId = aadhaarFrontImageId,
        Beam.aadhaarNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber,
        Beam.aadhaarNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber,
        Beam.aadhaarNumber = Nothing,
        Beam.blocked = blocked,
        Beam.businessLicenseImageId = businessLicenseImageId,
        Beam.businessLicenseNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted businessLicenseNumber,
        Beam.businessLicenseNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash businessLicenseNumber,
        Beam.businessLicenseNumber = Nothing,
        Beam.enabled = enabled,
        Beam.fleetOwnerPersonId = Kernel.Types.Id.getId fleetOwnerPersonId,
        Beam.fleetType = fleetType,
        Beam.gstImageId = gstImageId,
        Beam.gstNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted gstNumber,
        Beam.gstNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash gstNumber,
        Beam.gstNumber = Nothing,
        Beam.isEligibleForSubscription = Just isEligibleForSubscription,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.panImageId = panImageId,
        Beam.panNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber,
        Beam.panNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber,
        Beam.panNumber = Nothing,
        Beam.referredByOperatorId = referredByOperatorId,
        Beam.registeredAt = registeredAt,
        Beam.verified = verified,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
