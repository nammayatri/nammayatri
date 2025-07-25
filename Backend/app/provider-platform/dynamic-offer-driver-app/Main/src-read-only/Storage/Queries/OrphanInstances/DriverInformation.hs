{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverInformation where

import qualified Data.Aeson
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
import qualified Storage.Queries.Transformers.FleetOwnerInformation
import qualified Storage.Queries.Transformers.Ride

instance FromTType' Beam.DriverInformation Domain.Types.DriverInformation.DriverInformation where
  fromTType' (Beam.DriverInformationT {..}) = do
    servicesEnabledForSubscription' <- Storage.Queries.Transformers.DriverInformation.backfillServiceEnabledForSubscription driverId servicesEnabledForSubscription
    pure $
      Just
        Domain.Types.DriverInformation.DriverInformation
          { aadhaarNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem aadhaarNumberEncrypted aadhaarNumberHash,
            aadhaarVerified = aadhaarVerified,
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
            canSwitchToIntraCity = Kernel.Prelude.fromMaybe Kernel.Prelude.True canSwitchToIntraCity,
            canSwitchToRental = Kernel.Prelude.fromMaybe Kernel.Prelude.False canSwitchToRental,
            compAadhaarImagePath = compAadhaarImagePath,
            dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
            dlNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem dlNumberEncrypted dlNumberHash,
            driverDob = driverDob,
            driverFlowStatus = driverFlowStatus,
            driverId = Kernel.Types.Id.Id driverId,
            driverTripEndLocation = Storage.Queries.Transformers.Ride.mkLatLong driverTripEndLocationLat driverTripEndLocationLon,
            drunkAndDriveViolationCount = drunkAndDriveViolationCount,
            enabled = enabled,
            enabledAt = enabledAt,
            extraFareMitigationFlag = extraFareMitigationFlag,
            forwardBatchingEnabled = Kernel.Prelude.fromMaybe Kernel.Prelude.False forwardBatchingEnabled,
            hasAdvanceBooking = Kernel.Prelude.fromMaybe Kernel.Prelude.False hasAdvanceBooking,
            hasRideStarted = hasRideStarted,
            isBlockedForReferralPayout = isBlockedForReferralPayout,
            isInteroperable = Kernel.Prelude.fromMaybe Kernel.Prelude.False isInteroperable,
            isPetModeEnabled = Kernel.Prelude.fromMaybe Kernel.Prelude.False isPetModeEnabled,
            isSpecialLocWarrior = Kernel.Prelude.fromMaybe Kernel.Prelude.False isSpecialLocWarrior,
            issueBreachCooldownTimes = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< issueBreachCooldownTimes,
            lastACStatusCheckedAt = lastACStatusCheckedAt,
            lastEnabledOn = lastEnabledOn,
            latestScheduledBooking = latestScheduledBooking,
            latestScheduledPickup = Storage.Queries.Transformers.Ride.mkLatLong latestScheduledPickupLat latestScheduledPickupLon,
            mode = mode,
            numOfLocks = numOfLocks,
            onRide = onRide,
            onRideTripCategory = onRideTripCategory,
            onboardingVehicleCategory = onboardingVehicleCategory,
            panNumber = Storage.Queries.Transformers.FleetOwnerInformation.mkEncryptedItem panNumberEncrypted panNumberHash,
            payerVpa = payerVpa,
            paymentPending = paymentPending,
            payoutRegAmountRefunded = payoutRegAmountRefunded,
            payoutRegistrationOrderId = payoutRegistrationOrderId,
            payoutVpa = payoutVpa,
            payoutVpaBankAccount = payoutVpaBankAccount,
            payoutVpaStatus = payoutVpaStatus,
            preferredPrimarySpecialLocId = Kernel.Types.Id.Id <$> preferredPrimarySpecialLocId,
            preferredSecondarySpecialLocIds = Kernel.Prelude.maybe [] (map Kernel.Types.Id.Id) preferredSecondarySpecialLocIds,
            referralCode = referralCode,
            referredByDriverId = Kernel.Types.Id.Id <$> referredByDriverId,
            referredByFleetOwnerId = referredByFleetOwnerId,
            referredByOperatorId = referredByOperatorId,
            servicesEnabledForSubscription = servicesEnabledForSubscription',
            softBlockExpiryTime = softBlockExpiryTime,
            softBlockReasonFlag = softBlockReasonFlag,
            softBlockStiers = softBlockStiers,
            specialLocWarriorEnabledAt = specialLocWarriorEnabledAt,
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
      { Beam.aadhaarNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber,
        Beam.aadhaarNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber,
        Beam.aadhaarVerified = aadhaarVerified,
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
        Beam.canSwitchToIntraCity = Kernel.Prelude.Just canSwitchToIntraCity,
        Beam.canSwitchToRental = Kernel.Prelude.Just canSwitchToRental,
        Beam.compAadhaarImagePath = compAadhaarImagePath,
        Beam.dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown,
        Beam.dlNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted dlNumber,
        Beam.dlNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash dlNumber,
        Beam.driverDob = driverDob,
        Beam.driverFlowStatus = driverFlowStatus,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.driverTripEndLocationLat = Kernel.Prelude.fmap (.lat) driverTripEndLocation,
        Beam.driverTripEndLocationLon = Kernel.Prelude.fmap (.lon) driverTripEndLocation,
        Beam.drunkAndDriveViolationCount = drunkAndDriveViolationCount,
        Beam.enabled = enabled,
        Beam.enabledAt = enabledAt,
        Beam.extraFareMitigationFlag = extraFareMitigationFlag,
        Beam.forwardBatchingEnabled = Kernel.Prelude.Just forwardBatchingEnabled,
        Beam.hasAdvanceBooking = Kernel.Prelude.Just hasAdvanceBooking,
        Beam.hasRideStarted = hasRideStarted,
        Beam.isBlockedForReferralPayout = isBlockedForReferralPayout,
        Beam.isInteroperable = Kernel.Prelude.Just isInteroperable,
        Beam.isPetModeEnabled = Kernel.Prelude.Just isPetModeEnabled,
        Beam.isSpecialLocWarrior = Kernel.Prelude.Just isSpecialLocWarrior,
        Beam.issueBreachCooldownTimes = Kernel.Prelude.toJSON <$> issueBreachCooldownTimes,
        Beam.lastACStatusCheckedAt = lastACStatusCheckedAt,
        Beam.lastEnabledOn = lastEnabledOn,
        Beam.latestScheduledBooking = latestScheduledBooking,
        Beam.latestScheduledPickupLat = Kernel.Prelude.fmap (.lat) latestScheduledPickup,
        Beam.latestScheduledPickupLon = Kernel.Prelude.fmap (.lon) latestScheduledPickup,
        Beam.mode = mode,
        Beam.numOfLocks = numOfLocks,
        Beam.onRide = onRide,
        Beam.onRideTripCategory = onRideTripCategory,
        Beam.onboardingVehicleCategory = onboardingVehicleCategory,
        Beam.panNumberEncrypted = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber,
        Beam.panNumberHash = Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber,
        Beam.payerVpa = payerVpa,
        Beam.paymentPending = paymentPending,
        Beam.payoutRegAmountRefunded = payoutRegAmountRefunded,
        Beam.payoutRegistrationOrderId = payoutRegistrationOrderId,
        Beam.payoutVpa = payoutVpa,
        Beam.payoutVpaBankAccount = payoutVpaBankAccount,
        Beam.payoutVpaStatus = payoutVpaStatus,
        Beam.preferredPrimarySpecialLocId = Kernel.Types.Id.getId <$> preferredPrimarySpecialLocId,
        Beam.preferredSecondarySpecialLocIds = Kernel.Prelude.Just (map Kernel.Types.Id.getId preferredSecondarySpecialLocIds),
        Beam.referralCode = referralCode,
        Beam.referredByDriverId = Kernel.Types.Id.getId <$> referredByDriverId,
        Beam.referredByFleetOwnerId = referredByFleetOwnerId,
        Beam.referredByOperatorId = referredByOperatorId,
        Beam.servicesEnabledForSubscription = Kernel.Prelude.Just servicesEnabledForSubscription,
        Beam.softBlockExpiryTime = softBlockExpiryTime,
        Beam.softBlockReasonFlag = softBlockReasonFlag,
        Beam.softBlockStiers = softBlockStiers,
        Beam.specialLocWarriorEnabledAt = specialLocWarriorEnabledAt,
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
