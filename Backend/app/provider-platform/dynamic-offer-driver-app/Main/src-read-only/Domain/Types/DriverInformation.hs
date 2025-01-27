{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverInformation where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.VehicleCategory
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.BehaviourManagement.IssueBreach
import qualified Tools.Beam.UtilsTH
import qualified Tools.Error

data DriverInformation = DriverInformation
  { aadhaarVerified :: Kernel.Prelude.Bool,
    acRestrictionLiftCount :: Kernel.Prelude.Int,
    acUsageRestrictionType :: Domain.Types.DriverInformation.AirConditionedRestrictionType,
    active :: Kernel.Prelude.Bool,
    adminId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    airConditionScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    autoPayStatus :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverAutoPayStatus,
    availableUpiApps :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    blockReasonFlag :: Kernel.Prelude.Maybe Tools.Error.BlockReasonFlag,
    blockStateModifier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blocked :: Kernel.Prelude.Bool,
    blockedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    canDowngradeToHatchback :: Kernel.Prelude.Bool,
    canDowngradeToSedan :: Kernel.Prelude.Bool,
    canDowngradeToTaxi :: Kernel.Prelude.Bool,
    canSwitchToInterCity :: Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Bool,
    compAadhaarImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dailyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverTripEndLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    enabled :: Kernel.Prelude.Bool,
    enabledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    extraFareMitigationFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    forwardBatchingEnabled :: Kernel.Prelude.Bool,
    hasAdvanceBooking :: Kernel.Prelude.Bool,
    hasRideStarted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedForReferralPayout :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isInteroperable :: Kernel.Prelude.Bool,
    isSpecialLocWarrior :: Kernel.Prelude.Bool,
    issueBreachCooldownTimes :: Kernel.Prelude.Maybe [SharedLogic.BehaviourManagement.IssueBreach.IssueBreachCooldownTime],
    lastACStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastEnabledOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    latestScheduledBooking :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    latestScheduledPickup :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    mode :: Kernel.Prelude.Maybe Domain.Types.Common.DriverMode,
    numOfLocks :: Kernel.Prelude.Int,
    onRide :: Kernel.Prelude.Bool,
    onRideTripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    onboardingVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentPending :: Kernel.Prelude.Bool,
    payoutRegAmountRefunded :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutVpaBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutVpaStatus :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.PayoutVpaStatus,
    preferredPrimarySpecialLocId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation),
    preferredSecondarySpecialLocIds :: [Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation],
    referralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referredByDriverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    softBlockExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    softBlockReasonFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    softBlockStiers :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    specialLocWarriorEnabledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscribed :: Kernel.Prelude.Bool,
    tollRelatedIssueCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    totalReferred :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    verified :: Kernel.Prelude.Bool,
    weeklyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data AirConditionedRestrictionType = NoRestriction | ToggleAllowed | ToggleNotAllowed deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data Badges = Badges {badgeCount :: Kernel.Prelude.Int, badgeName :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverAutoPayStatus
  = PENDING
  | ACTIVE
  | SUSPENDED
  | PAUSED_PSP
  | CANCELLED_PSP
  | MANDATE_FAILED
  | MANDATE_EXPIRED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

newtype DriverBadges = DriverBadges {driverBadges :: [Domain.Types.DriverInformation.Badges]} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverMissedOpp = DriverMissedOpp
  { cancellationRate :: Kernel.Prelude.Int,
    missedEarnings :: Kernel.Types.Common.Money,
    missedEarningsWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    ridesCancelled :: Kernel.Prelude.Int,
    totalRides :: Kernel.Prelude.Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverSummary = DriverSummary
  { bonusEarned :: Kernel.Types.Common.Money,
    bonusEarnedWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    lastRegistered :: Kernel.Prelude.UTCTime,
    lateNightTrips :: Kernel.Prelude.Int,
    totalCompletedTrips :: Kernel.Prelude.Int,
    totalEarnings :: Kernel.Types.Common.Money,
    totalEarningsWithCurrency :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PayoutVpaStatus = VIA_WEBHOOK | MANUALLY_ADDED | VERIFIED_BY_USER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AirConditionedRestrictionType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverAutoPayStatus)

$(mkHttpInstancesForEnum ''DriverAutoPayStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutVpaStatus)
