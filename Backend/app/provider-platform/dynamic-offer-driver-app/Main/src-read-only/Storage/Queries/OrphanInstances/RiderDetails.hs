{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.RiderDetails where

import qualified Domain.Types.RiderDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.RiderDetails as Beam

instance FromTType' Beam.RiderDetails Domain.Types.RiderDetails.RiderDetails where
  fromTType' (Beam.RiderDetailsT {..}) = do
    pure $
      Just
        Domain.Types.RiderDetails.RiderDetails
          { bapId = bapId,
            cancellationDues = cancellationDues,
            createdAt = createdAt,
            currency = fromMaybe Kernel.Types.Common.INR currency,
            disputeChancesUsed = disputeChancesUsed,
            firstRideId = firstRideId,
            hasTakenValidRide = hasTakenValidRide,
            hasTakenValidRideAt = hasTakenValidRideAt,
            id = Kernel.Types.Id.Id id,
            isDeviceIdExists = isDeviceIdExists,
            isFlagConfirmed = isFlagConfirmed,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            nightSafetyChecks = nightSafetyChecks,
            otpCode = otpCode,
            payoutFlagReason = payoutFlagReason,
            referralCode = Kernel.Types.Id.Id <$> referralCode,
            referredAt = referredAt,
            referredByDriver = Kernel.Types.Id.Id <$> referredByDriver,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderDetails Domain.Types.RiderDetails.RiderDetails where
  toTType' (Domain.Types.RiderDetails.RiderDetails {..}) = do
    Beam.RiderDetailsT
      { Beam.bapId = bapId,
        Beam.cancellationDues = cancellationDues,
        Beam.createdAt = createdAt,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.disputeChancesUsed = disputeChancesUsed,
        Beam.firstRideId = firstRideId,
        Beam.hasTakenValidRide = hasTakenValidRide,
        Beam.hasTakenValidRideAt = hasTakenValidRideAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isDeviceIdExists = isDeviceIdExists,
        Beam.isFlagConfirmed = isFlagConfirmed,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.nightSafetyChecks = nightSafetyChecks,
        Beam.otpCode = otpCode,
        Beam.payoutFlagReason = payoutFlagReason,
        Beam.referralCode = Kernel.Types.Id.getId <$> referralCode,
        Beam.referredAt = referredAt,
        Beam.referredByDriver = Kernel.Types.Id.getId <$> referredByDriver,
        Beam.updatedAt = updatedAt
      }
