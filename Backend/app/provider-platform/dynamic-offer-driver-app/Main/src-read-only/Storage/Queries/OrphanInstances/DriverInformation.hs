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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverInformation as Beam
import qualified Storage.Queries.Transformers.DriverInformation
import qualified Storage.Queries.Transformers.Ride

instance FromTType' Beam.DriverInformation Domain.Types.DriverInformation.DriverInformation where
  fromTType' (Beam.DriverInformationT {..}) = do
    preferredPrimarySpecialLoc' <- Storage.Queries.Transformers.DriverInformation.getPreferredPrimarySpecialLoc preferredPrimarySpecialLocId
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
            blockReasonFlag = blockReasonFlag,
            blockStateModifier = blockStateModifier,
            blocked = blocked,
            blockedReason = blockedReason,
            canDowngradeToHatchback = canDowngradeToHatchback,
            canDowngradeToSedan = canDowngradeToSedan,
            canDowngradeToTaxi = canDowngradeToTaxi,
            canSwitchToInterCity = Kernel.Prelude.fromMaybe Kernel.Prelude.False canSwitchToInterCity,
            canSwitchToRental = Kernel.Prelude.fromMaybe Kernel.Prelude.False canSwitchToRental,
            compAadhaarImagePath = compAadhaarImagePath,
            dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
            driverDob = driverDob,
            driverId = Kernel.Types.Id.Id driverId,
            driverTripEndLocation = Storage.Queries.Transformers.Ride.mkLatLong driverTripEndLocationLat driverTripEndLocationLon,
            enabled = enabled,
            enabledAt = enabledAt,
            forwardBatchingEnabled = Kernel.Prelude.fromMaybe Kernel.Prelude.False forwardBatchingEnabled,
            hasAdvanceBooking = Kernel.Prelude.fromMaybe Kernel.Prelude.False hasAdvanceBooking,
            isInteroperable = Kernel.Prelude.fromMaybe Kernel.Prelude.False isInteroperable,
            isSpecialLocWarrior = Kernel.Prelude.fromMaybe Kernel.Prelude.False isSpecialLocWarrior,
            lastACStatusCheckedAt = lastACStatusCheckedAt,
            lastEnabledOn = lastEnabledOn,
            latestScheduledBooking = latestScheduledBooking,
            latestScheduledPickup = Storage.Queries.Transformers.Ride.mkLatLong latestScheduledPickupLat latestScheduledPickupLon,
            mode = mode,
            numOfLocks = numOfLocks,
            onRide = onRide,
            onRideTripCategory = onRideTripCategory,
            payerVpa = payerVpa,
            paymentPending = paymentPending,
            payoutRegAmountRefunded = payoutRegAmountRefunded,
            payoutRegistrationOrderId = payoutRegistrationOrderId,
            payoutVpa = payoutVpa,
            payoutVpaBankAccount = payoutVpaBankAccount,
            payoutVpaStatus = payoutVpaStatus,
            preferredPrimarySpecialLoc = preferredPrimarySpecialLoc',
            preferredSecondarySpecialLocIds = Kernel.Prelude.maybe [] (map Kernel.Types.Id.Id) preferredSecondarySpecialLocIds,
            referralCode = referralCode,
            referredByDriverId = Kernel.Types.Id.Id <$> referredByDriverId,
            subscribed = subscribed,
            tollRelatedIssueCount = tollRelatedIssueCount,
            totalReferred = totalReferred,
            verified = verified,
            weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown,
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
        Beam.blockReasonFlag = blockReasonFlag,
        Beam.blockStateModifier = blockStateModifier,
        Beam.blocked = blocked,
        Beam.blockedReason = blockedReason,
        Beam.canDowngradeToHatchback = canDowngradeToHatchback,
        Beam.canDowngradeToSedan = canDowngradeToSedan,
        Beam.canDowngradeToTaxi = canDowngradeToTaxi,
        Beam.canSwitchToInterCity = Kernel.Prelude.Just canSwitchToInterCity,
        Beam.canSwitchToRental = Kernel.Prelude.Just canSwitchToRental,
        Beam.compAadhaarImagePath = compAadhaarImagePath,
        Beam.dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
        Beam.driverDob = driverDob,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverTripEndLocationLat = Kernel.Prelude.fmap (.lat) driverTripEndLocation,
        Beam.driverTripEndLocationLon = Kernel.Prelude.fmap (.lon) driverTripEndLocation,
        Beam.enabled = enabled,
        Beam.enabledAt = enabledAt,
        Beam.forwardBatchingEnabled = Kernel.Prelude.Just forwardBatchingEnabled,
        Beam.hasAdvanceBooking = Kernel.Prelude.Just hasAdvanceBooking,
        Beam.isInteroperable = Kernel.Prelude.Just isInteroperable,
        Beam.isSpecialLocWarrior = Kernel.Prelude.Just isSpecialLocWarrior,
        Beam.lastACStatusCheckedAt = lastACStatusCheckedAt,
        Beam.lastEnabledOn = lastEnabledOn,
        Beam.latestScheduledBooking = latestScheduledBooking,
        Beam.latestScheduledPickupLat = Kernel.Prelude.fmap (.lat) latestScheduledPickup,
        Beam.latestScheduledPickupLon = Kernel.Prelude.fmap (.lon) latestScheduledPickup,
        Beam.mode = mode,
        Beam.numOfLocks = numOfLocks,
        Beam.onRide = onRide,
        Beam.onRideTripCategory = onRideTripCategory,
        Beam.payerVpa = payerVpa,
        Beam.paymentPending = paymentPending,
        Beam.payoutRegAmountRefunded = payoutRegAmountRefunded,
        Beam.payoutRegistrationOrderId = payoutRegistrationOrderId,
        Beam.payoutVpa = payoutVpa,
        Beam.payoutVpaBankAccount = payoutVpaBankAccount,
        Beam.payoutVpaStatus = payoutVpaStatus,
        Beam.preferredPrimarySpecialLocId = ((Kernel.Types.Id.getId . (.id)) <$> preferredPrimarySpecialLoc),
        Beam.preferredSecondarySpecialLocIds = Kernel.Prelude.Just (map Kernel.Types.Id.getId preferredSecondarySpecialLocIds),
        Beam.referralCode = referralCode,
        Beam.referredByDriverId = Kernel.Types.Id.getId <$> referredByDriverId,
        Beam.subscribed = subscribed,
        Beam.tollRelatedIssueCount = tollRelatedIssueCount,
        Beam.totalReferred = totalReferred,
        Beam.verified = verified,
        Beam.weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
