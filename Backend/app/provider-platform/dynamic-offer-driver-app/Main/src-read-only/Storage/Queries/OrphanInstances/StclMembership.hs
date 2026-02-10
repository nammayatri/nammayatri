{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.StclMembership where

import qualified Domain.Types.StclMembership
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.StclMembership as Beam

instance FromTType' Beam.StclMembership Domain.Types.StclMembership.StclMembership where
  fromTType' (Beam.StclMembershipT {..}) = do
    pure $
      Just
        Domain.Types.StclMembership.StclMembership
          { accountNumber = EncryptedHashed (Encrypted accountNumberEncrypted) accountNumberHash,
            addressCity = addressCity,
            addressPostalCode = addressPostalCode,
            addressState = addressState,
            addressStreetAddress1 = addressStreetAddress1,
            addressStreetAddress2 = addressStreetAddress2,
            applicationCount = applicationCount,
            applicationId = applicationId,
            bankBranch = bankBranch,
            bankName = bankName,
            createdAt = createdAt,
            dateOfBirth = dateOfBirth,
            declarationDate = declarationDate,
            declarationPlace = declarationPlace,
            declarationSignature = declarationSignature,
            driverId = Kernel.Types.Id.Id driverId,
            emailId = emailId,
            fatherMotherName = fatherMotherName,
            firstName = firstName,
            fuelTypes = fuelTypes,
            id = Kernel.Types.Id.Id id,
            ifscCode = EncryptedHashed (Encrypted ifscCodeEncrypted) ifscCodeHash,
            lastName = lastName,
            memberCategory = memberCategory,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            nomineeName = nomineeName,
            numberOfShares = numberOfShares,
            panNumber = EncryptedHashed (Encrypted panNumberEncrypted) panNumberHash,
            paymentStatus = paymentStatus,
            shareEndCount = shareEndCount,
            shareStartCount = shareStartCount,
            shortId = shortId,
            status = status,
            termsAccepted = termsAccepted,
            updatedAt = updatedAt,
            vehicleType = vehicleType
          }

instance ToTType' Beam.StclMembership Domain.Types.StclMembership.StclMembership where
  toTType' (Domain.Types.StclMembership.StclMembership {..}) = do
    Beam.StclMembershipT
      { Beam.accountNumberEncrypted = unEncrypted . (.encrypted) $ accountNumber,
        Beam.accountNumberHash = (.hash) accountNumber,
        Beam.addressCity = addressCity,
        Beam.addressPostalCode = addressPostalCode,
        Beam.addressState = addressState,
        Beam.addressStreetAddress1 = addressStreetAddress1,
        Beam.addressStreetAddress2 = addressStreetAddress2,
        Beam.applicationCount = applicationCount,
        Beam.applicationId = applicationId,
        Beam.bankBranch = bankBranch,
        Beam.bankName = bankName,
        Beam.createdAt = createdAt,
        Beam.dateOfBirth = dateOfBirth,
        Beam.declarationDate = declarationDate,
        Beam.declarationPlace = declarationPlace,
        Beam.declarationSignature = declarationSignature,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.emailId = emailId,
        Beam.fatherMotherName = fatherMotherName,
        Beam.firstName = firstName,
        Beam.fuelTypes = fuelTypes,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.ifscCodeEncrypted = unEncrypted . (.encrypted) $ ifscCode,
        Beam.ifscCodeHash = (.hash) ifscCode,
        Beam.lastName = lastName,
        Beam.memberCategory = memberCategory,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mobileNumberEncrypted = unEncrypted . (.encrypted) $ mobileNumber,
        Beam.mobileNumberHash = (.hash) mobileNumber,
        Beam.nomineeName = nomineeName,
        Beam.numberOfShares = numberOfShares,
        Beam.panNumberEncrypted = unEncrypted . (.encrypted) $ panNumber,
        Beam.panNumberHash = (.hash) panNumber,
        Beam.paymentStatus = paymentStatus,
        Beam.shareEndCount = shareEndCount,
        Beam.shareStartCount = shareStartCount,
        Beam.shortId = shortId,
        Beam.status = status,
        Beam.termsAccepted = termsAccepted,
        Beam.updatedAt = updatedAt,
        Beam.vehicleType = vehicleType
      }
