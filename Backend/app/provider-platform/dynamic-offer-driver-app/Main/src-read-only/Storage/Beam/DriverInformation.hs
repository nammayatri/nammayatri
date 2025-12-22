{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverInformation where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.DriverFlowStatus
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH
import qualified Tools.Error

data DriverInformationT f = DriverInformationT
  { aadhaarNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    aadhaarNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    aadhaarVerified :: B.C f Kernel.Prelude.Bool,
    acRestrictionLiftCount :: B.C f Kernel.Prelude.Int,
    acUsageRestrictionType :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.AirConditionedRestrictionType),
    active :: B.C f Kernel.Prelude.Bool,
    adminId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    airConditionScore :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    autoPayStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus),
    availableUpiApps :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blockExpiryTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    blockReasonFlag :: B.C f (Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag),
    blockStateModifier :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    blocked :: B.C f Kernel.Prelude.Bool,
    blockedReason :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    canDowngradeToHatchback :: B.C f Kernel.Prelude.Bool,
    canDowngradeToSedan :: B.C f Kernel.Prelude.Bool,
    canDowngradeToTaxi :: B.C f Kernel.Prelude.Bool,
    canSwitchToInterCity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    canSwitchToIntraCity :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    canSwitchToRental :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    compAadhaarImagePath :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    dailyCancellationRateBlockingCooldown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    dlNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    dlNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    driverDob :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverFlowStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverFlowStatus.DriverFlowStatus),
    driverId :: B.C f Kernel.Prelude.Text,
    driverTripEndLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driverTripEndLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    drunkAndDriveViolationCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    enabled :: B.C f Kernel.Prelude.Bool,
    enabledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    extraFareMitigationFlag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    forwardBatchingEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasAdvanceBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    hasRideStarted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isBlockedForReferralPayout :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isHighAccuracyLocationEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isInteroperable :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isPetModeEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSilentModeEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isSpecialLocWarrior :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    isTTSEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    issueBreachCooldownTimes :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    lastACStatusCheckedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastEnabledOn :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    lastOfflineTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    latestScheduledBooking :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    latestScheduledPickupLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    latestScheduledPickupLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    maxPickupRadius :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    mode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.DriverMode),
    numOfLocks :: B.C f Kernel.Prelude.Int,
    onRide :: B.C f Kernel.Prelude.Bool,
    onRideTripCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.Common.TripCategory),
    onboardingVehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    onlineDurationRefreshedAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    panNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    panNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    payerVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    paymentPending :: B.C f Kernel.Prelude.Bool,
    payoutRegAmountRefunded :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    payoutRegistrationOrderId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutVpa :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutVpaBankAccount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    payoutVpaStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.PayoutVpaStatus),
    planExpiryDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    preferredPrimarySpecialLocId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    preferredSecondarySpecialLocIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    prepaidSubscriptionBalance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    referralCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredByDriverId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredByFleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    referredByOperatorId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rideRequestVolume :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    rideRequestVolumeEnabled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    ruleBasedUpgradeTiers :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    servicesEnabledForSubscription :: B.C f (Kernel.Prelude.Maybe [Domain.Types.Extra.Plan.ServiceNames]),
    softBlockExpiryTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    softBlockReasonFlag :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    softBlockStiers :: B.C f (Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType]),
    specialLocWarriorEnabledAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    subscribed :: B.C f Kernel.Prelude.Bool,
    tollRelatedIssueCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    tollRouteBlockedTill :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    totalReferred :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    tripDistanceMaxThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    tripDistanceMinThreshold :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters),
    verified :: B.C f Kernel.Prelude.Bool,
    walletBalance :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    weeklyCancellationRateBlockingCooldown :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f = DriverInformationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverInformationId . driverId

type DriverInformation = DriverInformationT Identity

$(enableKVPG ''DriverInformationT ['driverId] [])

$(mkTableInstances ''DriverInformationT "driver_information")
