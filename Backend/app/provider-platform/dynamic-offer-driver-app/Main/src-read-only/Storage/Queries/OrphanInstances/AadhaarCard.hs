{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.AadhaarCard where

import qualified Domain.Types.AadhaarCard
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.AadhaarCard as Beam

instance FromTType' Beam.AadhaarCard Domain.Types.AadhaarCard.AadhaarCard where
  fromTType' (Beam.AadhaarCardT {..}) = do
    pure $
      Just
        Domain.Types.AadhaarCard.AadhaarCard
          { aadhaarBackImageId = Kernel.Types.Id.Id <$> aadhaarBackImageId,
            aadhaarFrontImageId = Kernel.Types.Id.Id <$> aadhaarFrontImageId,
            aadhaarNumberHash = aadhaarNumberHash,
            address = address,
            consent = consent,
            consentTimestamp = consentTimestamp,
            createdAt = createdAt,
            dateOfBirth = dateOfBirth,
            driverGender = driverGender,
            driverId = Kernel.Types.Id.Id driverId,
            driverImage = driverImage,
            driverImagePath = driverImagePath,
            maskedAadhaarNumber = maskedAadhaarNumber,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nameOnCard = nameOnCard,
            updatedAt = updatedAt,
            verificationStatus = verificationStatus
          }

instance ToTType' Beam.AadhaarCard Domain.Types.AadhaarCard.AadhaarCard where
  toTType' (Domain.Types.AadhaarCard.AadhaarCard {..}) = do
    Beam.AadhaarCardT
      { Beam.aadhaarBackImageId = Kernel.Types.Id.getId <$> aadhaarBackImageId,
        Beam.aadhaarFrontImageId = Kernel.Types.Id.getId <$> aadhaarFrontImageId,
        Beam.aadhaarNumberHash = aadhaarNumberHash,
        Beam.address = address,
        Beam.consent = consent,
        Beam.consentTimestamp = consentTimestamp,
        Beam.createdAt = createdAt,
        Beam.dateOfBirth = dateOfBirth,
        Beam.driverGender = driverGender,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverImage = driverImage,
        Beam.driverImagePath = driverImagePath,
        Beam.maskedAadhaarNumber = maskedAadhaarNumber,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nameOnCard = nameOnCard,
        Beam.updatedAt = updatedAt,
        Beam.verificationStatus = verificationStatus
      }
