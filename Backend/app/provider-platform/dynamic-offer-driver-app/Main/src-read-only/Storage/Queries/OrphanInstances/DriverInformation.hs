{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverInformation where

import qualified Domain.Types.DriverInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverInformation as Beam

instance FromTType' Beam.DriverInformation Domain.Types.DriverInformation.DriverInformation where
  fromTType' (Beam.DriverInformationT {..}) = do
    pure $
      Just
        Domain.Types.DriverInformation.DriverInformation
          { aadhaarVerified = aadhaarVerified,
            acRestrictionLiftCount = acRestrictionLiftCount,
            acUsageRestrictionType = Kernel.Prelude.fromMaybe Domain.Types.DriverInformation.NoRestriction acUsageRestrictionType,
            active = active,
            adminId = Kernel.Types.Id.Id <$> adminId,
            airConditionScore = airConditionScore,
            autoPayStatus = autoPayStatus,
            availableUpiApps = availableUpiApps,
            blockExpiryTime = blockExpiryTime,
            blockStateModifier = blockStateModifier,
            blocked = blocked,
            blockedReason = blockedReason,
            canDowngradeToHatchback = canDowngradeToHatchback,
            canDowngradeToSedan = canDowngradeToSedan,
            canDowngradeToTaxi = canDowngradeToTaxi,
            canSwitchToInterCity = Kernel.Prelude.fromMaybe Kernel.Prelude.False canSwitchToInterCity,
            canSwitchToRental = Kernel.Prelude.fromMaybe Kernel.Prelude.False canSwitchToRental,
            compAadhaarImagePath = compAadhaarImagePath,
            driverDob = driverDob,
            driverId = Kernel.Types.Id.Id driverId,
            enabled = enabled,
            enabledAt = enabledAt,
            lastACStatusCheckedAt = lastACStatusCheckedAt,
            lastEnabledOn = lastEnabledOn,
            mode = mode,
            numOfLocks = numOfLocks,
            onRide = onRide,
            payerVpa = payerVpa,
            paymentPending = paymentPending,
            referralCode = referralCode,
            referredByDriverId = Kernel.Types.Id.Id <$> referredByDriverId,
            subscribed = subscribed,
            totalReferred = totalReferred,
            verified = verified,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverInformation Domain.Types.DriverInformation.DriverInformation where
  toTType' (Domain.Types.DriverInformation.DriverInformation {..}) = do
    Beam.DriverInformationT
      { Beam.aadhaarVerified = aadhaarVerified,
        Beam.acRestrictionLiftCount = acRestrictionLiftCount,
        Beam.acUsageRestrictionType = Kernel.Prelude.Just acUsageRestrictionType,
        Beam.active = active,
        Beam.adminId = Kernel.Types.Id.getId <$> adminId,
        Beam.airConditionScore = airConditionScore,
        Beam.autoPayStatus = autoPayStatus,
        Beam.availableUpiApps = availableUpiApps,
        Beam.blockExpiryTime = blockExpiryTime,
        Beam.blockStateModifier = blockStateModifier,
        Beam.blocked = blocked,
        Beam.blockedReason = blockedReason,
        Beam.canDowngradeToHatchback = canDowngradeToHatchback,
        Beam.canDowngradeToSedan = canDowngradeToSedan,
        Beam.canDowngradeToTaxi = canDowngradeToTaxi,
        Beam.canSwitchToInterCity = Kernel.Prelude.Just canSwitchToInterCity,
        Beam.canSwitchToRental = Kernel.Prelude.Just canSwitchToRental,
        Beam.compAadhaarImagePath = compAadhaarImagePath,
        Beam.driverDob = driverDob,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.enabled = enabled,
        Beam.enabledAt = enabledAt,
        Beam.lastACStatusCheckedAt = lastACStatusCheckedAt,
        Beam.lastEnabledOn = lastEnabledOn,
        Beam.mode = mode,
        Beam.numOfLocks = numOfLocks,
        Beam.onRide = onRide,
        Beam.payerVpa = payerVpa,
        Beam.paymentPending = paymentPending,
        Beam.referralCode = referralCode,
        Beam.referredByDriverId = Kernel.Types.Id.getId <$> referredByDriverId,
        Beam.subscribed = subscribed,
        Beam.totalReferred = totalReferred,
        Beam.verified = verified,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
