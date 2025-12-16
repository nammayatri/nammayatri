{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverInformation (module Storage.Queries.DriverInformation, module ReExport) where

import qualified Domain.Types.DriverInformation
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.UpgradedTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified SharedLogic.BehaviourManagement.IssueBreach
import qualified Storage.Beam.DriverInformation as Beam
import Storage.Queries.DriverInformationExtra as ReExport
import qualified Storage.Queries.Transformers.FleetOwnerInformation

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverInformation.DriverInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverInformation.DriverInformation] -> m ())
createMany = traverse_ create

addReferralCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
addReferralCode referralCode referredByDriverId driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.referralCode referralCode,
      Se.Set Beam.referredByDriverId (Kernel.Types.Id.getId <$> referredByDriverId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

incrementReferralCountByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
incrementReferralCountByPersonId totalReferred driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.totalReferred totalReferred, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

removeAcUsageRestriction ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Domain.Types.DriverInformation.AirConditionedRestrictionType -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
removeAcUsageRestriction airConditionScore acUsageRestrictionType acRestrictionLiftCount driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.acRestrictionLiftCount acRestrictionLiftCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateAadhaarVerifiedState :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAadhaarVerifiedState aadhaarVerified driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.aadhaarVerified aadhaarVerified, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateAcUsageRestrictionAndScore ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.DriverInformation.AirConditionedRestrictionType -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAcUsageRestrictionAndScore acUsageRestrictionType airConditionScore driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateAirConditionScore :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateAirConditionScore airConditionScore driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.airConditionScore airConditionScore, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateCompAadhaarImagePath :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateCompAadhaarImagePath compAadhaarImagePath driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.compAadhaarImagePath compAadhaarImagePath, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDailyAndWeeklyCancellationRateBlockingCooldown ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDailyAndWeeklyCancellationRateBlockingCooldown dailyCancellationRateBlockingCooldown weeklyCancellationRateBlockingCooldown driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.dailyCancellationRateBlockingCooldown dailyCancellationRateBlockingCooldown,
      Se.Set Beam.weeklyCancellationRateBlockingCooldown weeklyCancellationRateBlockingCooldown,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDailyAndWeeklyExtraKms ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDailyAndWeeklyExtraKms dailyExtraKms weeklyExtraKms driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.dailyExtraKms dailyExtraKms, Se.Set Beam.weeklyExtraKms weeklyExtraKms, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDailyCancellationRateBlockingCooldown :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDailyCancellationRateBlockingCooldown dailyCancellationRateBlockingCooldown driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.dailyCancellationRateBlockingCooldown dailyCancellationRateBlockingCooldown, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDriverDob :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverDob driverDob driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverDob driverDob, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDriverDowngradeForSuv :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverDowngradeForSuv canDowngradeToHatchback canDowngradeToTaxi driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDriverInformation ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.DriverInformation.OnboardingAs -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity canSwitchToIntraCity availableUpiApps isPetModeEnabled tripDistanceMaxThreshold tripDistanceMinThreshold maxPickupRadius isSilentModeEnabled rideRequestVolume isTTSEnabled isHighAccuracyLocationEnabled rideRequestVolumeEnabled onboardingAs driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canDowngradeToSedan canDowngradeToSedan,
      Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.canSwitchToIntraCity (Kernel.Prelude.Just canSwitchToIntraCity),
      Se.Set Beam.availableUpiApps availableUpiApps,
      Se.Set Beam.isPetModeEnabled (Kernel.Prelude.Just isPetModeEnabled),
      Se.Set Beam.tripDistanceMaxThreshold tripDistanceMaxThreshold,
      Se.Set Beam.tripDistanceMinThreshold tripDistanceMinThreshold,
      Se.Set Beam.maxPickupRadius maxPickupRadius,
      Se.Set Beam.isSilentModeEnabled isSilentModeEnabled,
      Se.Set Beam.rideRequestVolume rideRequestVolume,
      Se.Set Beam.isTTSEnabled isTTSEnabled,
      Se.Set Beam.isHighAccuracyLocationEnabled isHighAccuracyLocationEnabled,
      Se.Set Beam.rideRequestVolumeEnabled rideRequestVolumeEnabled,
      Se.Set Beam.onboardingAs onboardingAs,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateDrunkAndDriveViolationCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDrunkAndDriveViolationCount drunkAndDriveViolationCount driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.drunkAndDriveViolationCount drunkAndDriveViolationCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateExtraFareMitigation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateExtraFareMitigation extraFareMitigationFlag driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.extraFareMitigationFlag extraFareMitigationFlag, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateForwardBatchingEnabled :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateForwardBatchingEnabled forwardBatchingEnabled driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.forwardBatchingEnabled (Kernel.Prelude.Just forwardBatchingEnabled), Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateIsInteroperable :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateIsInteroperable isInteroperable driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isInteroperable (Kernel.Prelude.Just isInteroperable), Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateIssueBreachCooldownTimes ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [SharedLogic.BehaviourManagement.IssueBreach.IssueBreachCooldownTime] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateIssueBreachCooldownTimes issueBreachCooldownTimes driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.issueBreachCooldownTimes (Kernel.Prelude.toJSON <$> issueBreachCooldownTimes), Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateLastACStatusCheckedAt :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateLastACStatusCheckedAt lastACStatusCheckedAt driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.lastACStatusCheckedAt lastACStatusCheckedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateLatestScheduledBookingAndPickup ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.External.Maps.LatLong -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateLatestScheduledBookingAndPickup latestScheduledBooking latestScheduledPickup driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.latestScheduledBooking latestScheduledBooking,
      Se.Set Beam.latestScheduledPickupLat (Kernel.Prelude.fmap (.lat) latestScheduledPickup),
      Se.Set Beam.latestScheduledPickupLon (Kernel.Prelude.fmap (.lon) latestScheduledPickup),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateOnRide :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateOnRide onRide driverId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.onRide onRide, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateOnRideAndLatestScheduledBookingAndPickup ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.External.Maps.LatLong -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateOnRideAndLatestScheduledBookingAndPickup onRide latestScheduledBooking latestScheduledPickup driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.onRide onRide,
      Se.Set Beam.latestScheduledBooking latestScheduledBooking,
      Se.Set Beam.latestScheduledPickupLat (Kernel.Prelude.fmap (.lat) latestScheduledPickup),
      Se.Set Beam.latestScheduledPickupLon (Kernel.Prelude.fmap (.lon) latestScheduledPickup),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updatePayoutRegAmountRefunded :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePayoutRegAmountRefunded payoutRegAmountRefunded driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutRegAmountRefunded payoutRegAmountRefunded, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updatePayoutRegistrationOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePayoutRegistrationOrderId payoutRegistrationOrderId driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutRegistrationOrderId payoutRegistrationOrderId, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updatePayoutVpaAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.DriverInformation.PayoutVpaStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePayoutVpaAndStatus payoutVpa payoutVpaStatus driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutVpa payoutVpa, Se.Set Beam.payoutVpaStatus payoutVpaStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updatePayoutVpaStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Domain.Types.DriverInformation.PayoutVpaStatus -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePayoutVpaStatus payoutVpaStatus driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.payoutVpaStatus payoutVpaStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updatePendingPayment :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePendingPayment paymentPending driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.paymentPending paymentPending, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateReferredByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByFleetOwnerId referredByFleetOwnerId driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referredByFleetOwnerId referredByFleetOwnerId, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateReferredByOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateReferredByOperatorId referredByOperatorId driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.referredByOperatorId referredByOperatorId, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateRentalInterCityAndIntraCitySwitch ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateRentalInterCityAndIntraCitySwitch canSwitchToRental canSwitchToInterCity canSwitchToIntraCity driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.canSwitchToIntraCity (Kernel.Prelude.Just canSwitchToIntraCity),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateServicesEnabledForSubscription :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Extra.Plan.ServiceNames] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateServicesEnabledForSubscription servicesEnabledForSubscription driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.servicesEnabledForSubscription (Kernel.Prelude.Just servicesEnabledForSubscription),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateSoftBlock ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType] -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateSoftBlock softBlockStiers softBlockExpiryTime softBlockReasonFlag driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.softBlockStiers softBlockStiers,
      Se.Set Beam.softBlockExpiryTime softBlockExpiryTime,
      Se.Set Beam.softBlockReasonFlag softBlockReasonFlag,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateSpecialLocWarriorInfo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> [Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation] -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocId preferredSecondarySpecialLocIds specialLocWarriorEnabledAt driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.isSpecialLocWarrior (Kernel.Prelude.Just isSpecialLocWarrior),
      Se.Set Beam.preferredPrimarySpecialLocId (Kernel.Types.Id.getId <$> preferredPrimarySpecialLocId),
      Se.Set Beam.preferredSecondarySpecialLocIds (Kernel.Prelude.Just (map Kernel.Types.Id.getId preferredSecondarySpecialLocIds)),
      Se.Set Beam.specialLocWarriorEnabledAt specialLocWarriorEnabledAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateSubscription :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateSubscription subscribed driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.subscribed subscribed, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateTollRelatedIssueCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateTollRelatedIssueCount tollRelatedIssueCount driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.tollRelatedIssueCount tollRelatedIssueCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateTripEndLocation :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.External.Maps.LatLong -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateTripEndLocation driverTripEndLocation driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.driverTripEndLocationLat (Kernel.Prelude.fmap (.lat) driverTripEndLocation),
      Se.Set Beam.driverTripEndLocationLon (Kernel.Prelude.fmap (.lon) driverTripEndLocation),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateUpgradedTiers :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Domain.Types.UpgradedTier.UpgradedTier] -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateUpgradedTiers ruleBasedUpgradeTiers driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.ruleBasedUpgradeTiers (Kernel.Prelude.toJSON <$> ruleBasedUpgradeTiers), Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateWalletBalance :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateWalletBalance walletBalance driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.walletBalance walletBalance, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateWeeklyCancellationRateBlockingCooldown :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateWeeklyCancellationRateBlockingCooldown weeklyCancellationRateBlockingCooldown driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.weeklyCancellationRateBlockingCooldown weeklyCancellationRateBlockingCooldown, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverInformation.DriverInformation))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverInformation.DriverInformation -> m ())
updateByPrimaryKey (Domain.Types.DriverInformation.DriverInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.aadhaarNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted aadhaarNumber),
      Se.Set Beam.aadhaarNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash aadhaarNumber),
      Se.Set Beam.aadhaarVerified aadhaarVerified,
      Se.Set Beam.acRestrictionLiftCount acRestrictionLiftCount,
      Se.Set Beam.acUsageRestrictionType (Kernel.Prelude.Just acUsageRestrictionType),
      Se.Set Beam.active active,
      Se.Set Beam.adminId (Kernel.Types.Id.getId <$> adminId),
      Se.Set Beam.airConditionScore airConditionScore,
      Se.Set Beam.autoPayStatus autoPayStatus,
      Se.Set Beam.availableUpiApps availableUpiApps,
      Se.Set Beam.blockExpiryTime blockExpiryTime,
      Se.Set Beam.blockReasonFlag blockReasonFlag,
      Se.Set Beam.blockStateModifier blockStateModifier,
      Se.Set Beam.blocked blocked,
      Se.Set Beam.blockedReason blockedReason,
      Se.Set Beam.canDowngradeToHatchback canDowngradeToHatchback,
      Se.Set Beam.canDowngradeToSedan canDowngradeToSedan,
      Se.Set Beam.canDowngradeToTaxi canDowngradeToTaxi,
      Se.Set Beam.canSwitchToInterCity (Kernel.Prelude.Just canSwitchToInterCity),
      Se.Set Beam.canSwitchToIntraCity (Kernel.Prelude.Just canSwitchToIntraCity),
      Se.Set Beam.canSwitchToRental (Kernel.Prelude.Just canSwitchToRental),
      Se.Set Beam.compAadhaarImagePath compAadhaarImagePath,
      Se.Set Beam.dailyCancellationRateBlockingCooldown dailyCancellationRateBlockingCooldown,
      Se.Set Beam.dailyExtraKms dailyExtraKms,
      Se.Set Beam.dlNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted dlNumber),
      Se.Set Beam.dlNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash dlNumber),
      Se.Set Beam.driverDob driverDob,
      Se.Set Beam.driverFlowStatus driverFlowStatus,
      Se.Set Beam.driverTripEndLocationLat (Kernel.Prelude.fmap (.lat) driverTripEndLocation),
      Se.Set Beam.driverTripEndLocationLon (Kernel.Prelude.fmap (.lon) driverTripEndLocation),
      Se.Set Beam.drunkAndDriveViolationCount drunkAndDriveViolationCount,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.enabledAt enabledAt,
      Se.Set Beam.extraFareMitigationFlag extraFareMitigationFlag,
      Se.Set Beam.forwardBatchingEnabled (Kernel.Prelude.Just forwardBatchingEnabled),
      Se.Set Beam.hasAdvanceBooking (Kernel.Prelude.Just hasAdvanceBooking),
      Se.Set Beam.hasRideStarted hasRideStarted,
      Se.Set Beam.isBlockedForReferralPayout isBlockedForReferralPayout,
      Se.Set Beam.isHighAccuracyLocationEnabled isHighAccuracyLocationEnabled,
      Se.Set Beam.isInteroperable (Kernel.Prelude.Just isInteroperable),
      Se.Set Beam.isPetModeEnabled (Kernel.Prelude.Just isPetModeEnabled),
      Se.Set Beam.isSilentModeEnabled isSilentModeEnabled,
      Se.Set Beam.isSpecialLocWarrior (Kernel.Prelude.Just isSpecialLocWarrior),
      Se.Set Beam.isTTSEnabled isTTSEnabled,
      Se.Set Beam.issueBreachCooldownTimes (Kernel.Prelude.toJSON <$> issueBreachCooldownTimes),
      Se.Set Beam.lastACStatusCheckedAt lastACStatusCheckedAt,
      Se.Set Beam.lastEnabledOn lastEnabledOn,
      Se.Set Beam.lastOfflineTime lastOfflineTime,
      Se.Set Beam.latestScheduledBooking latestScheduledBooking,
      Se.Set Beam.latestScheduledPickupLat (Kernel.Prelude.fmap (.lat) latestScheduledPickup),
      Se.Set Beam.latestScheduledPickupLon (Kernel.Prelude.fmap (.lon) latestScheduledPickup),
      Se.Set Beam.maxPickupRadius maxPickupRadius,
      Se.Set Beam.mode mode,
      Se.Set Beam.numOfLocks numOfLocks,
      Se.Set Beam.onRide onRide,
      Se.Set Beam.onRideTripCategory onRideTripCategory,
      Se.Set Beam.onboardingAs onboardingAs,
      Se.Set Beam.onboardingVehicleCategory onboardingVehicleCategory,
      Se.Set Beam.onlineDurationRefreshedAt onlineDurationRefreshedAt,
      Se.Set Beam.panNumberEncrypted (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldEncrypted panNumber),
      Se.Set Beam.panNumberHash (Storage.Queries.Transformers.FleetOwnerInformation.mkFieldHash panNumber),
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.paymentPending paymentPending,
      Se.Set Beam.payoutRegAmountRefunded payoutRegAmountRefunded,
      Se.Set Beam.payoutRegistrationOrderId payoutRegistrationOrderId,
      Se.Set Beam.payoutVpa payoutVpa,
      Se.Set Beam.payoutVpaBankAccount payoutVpaBankAccount,
      Se.Set Beam.payoutVpaStatus payoutVpaStatus,
      Se.Set Beam.planExpiryDate planExpiryDate,
      Se.Set Beam.preferredPrimarySpecialLocId (Kernel.Types.Id.getId <$> preferredPrimarySpecialLocId),
      Se.Set Beam.preferredSecondarySpecialLocIds (Kernel.Prelude.Just (map Kernel.Types.Id.getId preferredSecondarySpecialLocIds)),
      Se.Set Beam.prepaidSubscriptionBalance prepaidSubscriptionBalance,
      Se.Set Beam.referralCode referralCode,
      Se.Set Beam.referredByDriverId (Kernel.Types.Id.getId <$> referredByDriverId),
      Se.Set Beam.referredByFleetOwnerId referredByFleetOwnerId,
      Se.Set Beam.referredByOperatorId referredByOperatorId,
      Se.Set Beam.rideRequestVolume rideRequestVolume,
      Se.Set Beam.rideRequestVolumeEnabled rideRequestVolumeEnabled,
      Se.Set Beam.ruleBasedUpgradeTiers (Kernel.Prelude.toJSON <$> ruleBasedUpgradeTiers),
      Se.Set Beam.servicesEnabledForSubscription (Kernel.Prelude.Just servicesEnabledForSubscription),
      Se.Set Beam.softBlockExpiryTime softBlockExpiryTime,
      Se.Set Beam.softBlockReasonFlag softBlockReasonFlag,
      Se.Set Beam.softBlockStiers softBlockStiers,
      Se.Set Beam.specialLocWarriorEnabledAt specialLocWarriorEnabledAt,
      Se.Set Beam.subscribed subscribed,
      Se.Set Beam.tollRelatedIssueCount tollRelatedIssueCount,
      Se.Set Beam.totalReferred totalReferred,
      Se.Set Beam.tripDistanceMaxThreshold tripDistanceMaxThreshold,
      Se.Set Beam.tripDistanceMinThreshold tripDistanceMinThreshold,
      Se.Set Beam.verified verified,
      Se.Set Beam.walletBalance walletBalance,
      Se.Set Beam.weeklyCancellationRateBlockingCooldown weeklyCancellationRateBlockingCooldown,
      Se.Set Beam.weeklyExtraKms weeklyExtraKms,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
