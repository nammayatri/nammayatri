{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverInformation where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.DriverFlowStatus
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.UpgradedTier
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Types.SpecialLocation
import qualified SharedLogic.BehaviourManagement.IssueBreach
import qualified Tools.Beam.UtilsTH
import qualified Tools.Error

data DriverInformationE e = DriverInformation
  { aadhaarNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    aadhaarVerified :: Kernel.Prelude.Bool,
    acRestrictionLiftCount :: Kernel.Prelude.Int,
    acUsageRestrictionType :: Domain.Types.DriverInformation.AirConditionedRestrictionType,
    active :: Kernel.Prelude.Bool,
    address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    addressDocumentType :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.AddressDocumentType,
    adminId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    airConditionScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    approved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
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
    canSwitchToIntraCity :: Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Bool,
    compAadhaarImagePath :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dailyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dailyExtraKms :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    dlNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverFlowStatus :: Kernel.Prelude.Maybe Domain.Types.DriverFlowStatus.DriverFlowStatus,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverTripEndLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    drunkAndDriveViolationCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    enabled :: Kernel.Prelude.Bool,
    enabledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    extraFareMitigationFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    forwardBatchingEnabled :: Kernel.Prelude.Bool,
    hasAdvanceBooking :: Kernel.Prelude.Bool,
    hasRideStarted :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isBlockedForReferralPayout :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isHighAccuracyLocationEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isInteroperable :: Kernel.Prelude.Bool,
    isPetModeEnabled :: Kernel.Prelude.Bool,
    isSilentModeEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isSpecialLocWarrior :: Kernel.Prelude.Bool,
    isTTSEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    issueBreachCooldownTimes :: Kernel.Prelude.Maybe [SharedLogic.BehaviourManagement.IssueBreach.IssueBreachCooldownTime],
    lastACStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastEnabledOn :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastOfflineTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    latestScheduledBooking :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    latestScheduledPickup :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    maxPickupRadius :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    mode :: Kernel.Prelude.Maybe Domain.Types.Common.DriverMode,
    numOfLocks :: Kernel.Prelude.Int,
    onRide :: Kernel.Prelude.Bool,
    onRideTripCategory :: Kernel.Prelude.Maybe Domain.Types.Common.TripCategory,
    onboardingAs :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.OnboardingAs,
    onboardingVehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    onlineDurationRefreshedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    panNumber :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
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
    referredByFleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referredByOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideRequestVolume :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    rideRequestVolumeEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    ruleBasedUpgradeTiers :: Kernel.Prelude.Maybe [Domain.Types.UpgradedTier.UpgradedTier],
    servicesEnabledForSubscription :: [Domain.Types.Extra.Plan.ServiceNames],
    softBlockExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    softBlockReasonFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    softBlockStiers :: Kernel.Prelude.Maybe [Domain.Types.ServiceTierType.ServiceTierType],
    specialLocWarriorEnabledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscribed :: Kernel.Prelude.Bool,
    tollRelatedIssueCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tollRouteBlockedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalReferred :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tripDistanceMaxThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    tripDistanceMinThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    verified :: Kernel.Prelude.Bool,
    weeklyCancellationRateBlockingCooldown :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    weeklyExtraKms :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type DriverInformation = DriverInformationE 'AsEncrypted

type DecryptedDriverInformation = DriverInformationE 'AsUnencrypted

instance EncryptedItem DriverInformation where
  type Unencrypted DriverInformation = (DecryptedDriverInformation, HashSalt)
  encryptItem (entity, salt) = do
    aadhaarNumber_ <- encryptItem $ (,salt) <$> aadhaarNumber entity
    dlNumber_ <- encryptItem $ (,salt) <$> dlNumber entity
    panNumber_ <- encryptItem $ (,salt) <$> panNumber entity
    pure
      DriverInformation
        { aadhaarNumber = aadhaarNumber_,
          aadhaarVerified = aadhaarVerified entity,
          acRestrictionLiftCount = acRestrictionLiftCount entity,
          acUsageRestrictionType = acUsageRestrictionType entity,
          active = active entity,
          address = address entity,
          addressDocumentType = addressDocumentType entity,
          adminId = adminId entity,
          airConditionScore = airConditionScore entity,
          approved = approved entity,
          autoPayStatus = autoPayStatus entity,
          availableUpiApps = availableUpiApps entity,
          blockExpiryTime = blockExpiryTime entity,
          blockReasonFlag = blockReasonFlag entity,
          blockStateModifier = blockStateModifier entity,
          blocked = blocked entity,
          blockedReason = blockedReason entity,
          canDowngradeToHatchback = canDowngradeToHatchback entity,
          canDowngradeToSedan = canDowngradeToSedan entity,
          canDowngradeToTaxi = canDowngradeToTaxi entity,
          canSwitchToInterCity = canSwitchToInterCity entity,
          canSwitchToIntraCity = canSwitchToIntraCity entity,
          canSwitchToRental = canSwitchToRental entity,
          compAadhaarImagePath = compAadhaarImagePath entity,
          dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown entity,
          dailyExtraKms = dailyExtraKms entity,
          dlNumber = dlNumber_,
          driverDob = driverDob entity,
          driverFlowStatus = driverFlowStatus entity,
          driverId = driverId entity,
          driverTripEndLocation = driverTripEndLocation entity,
          drunkAndDriveViolationCount = drunkAndDriveViolationCount entity,
          enabled = enabled entity,
          enabledAt = enabledAt entity,
          extraFareMitigationFlag = extraFareMitigationFlag entity,
          forwardBatchingEnabled = forwardBatchingEnabled entity,
          hasAdvanceBooking = hasAdvanceBooking entity,
          hasRideStarted = hasRideStarted entity,
          isBlockedForReferralPayout = isBlockedForReferralPayout entity,
          isHighAccuracyLocationEnabled = isHighAccuracyLocationEnabled entity,
          isInteroperable = isInteroperable entity,
          isPetModeEnabled = isPetModeEnabled entity,
          isSilentModeEnabled = isSilentModeEnabled entity,
          isSpecialLocWarrior = isSpecialLocWarrior entity,
          isTTSEnabled = isTTSEnabled entity,
          issueBreachCooldownTimes = issueBreachCooldownTimes entity,
          lastACStatusCheckedAt = lastACStatusCheckedAt entity,
          lastEnabledOn = lastEnabledOn entity,
          lastOfflineTime = lastOfflineTime entity,
          latestScheduledBooking = latestScheduledBooking entity,
          latestScheduledPickup = latestScheduledPickup entity,
          maxPickupRadius = maxPickupRadius entity,
          mode = mode entity,
          numOfLocks = numOfLocks entity,
          onRide = onRide entity,
          onRideTripCategory = onRideTripCategory entity,
          onboardingAs = onboardingAs entity,
          onboardingVehicleCategory = onboardingVehicleCategory entity,
          onlineDurationRefreshedAt = onlineDurationRefreshedAt entity,
          panNumber = panNumber_,
          payerVpa = payerVpa entity,
          paymentPending = paymentPending entity,
          payoutRegAmountRefunded = payoutRegAmountRefunded entity,
          payoutRegistrationOrderId = payoutRegistrationOrderId entity,
          payoutVpa = payoutVpa entity,
          payoutVpaBankAccount = payoutVpaBankAccount entity,
          payoutVpaStatus = payoutVpaStatus entity,
          preferredPrimarySpecialLocId = preferredPrimarySpecialLocId entity,
          preferredSecondarySpecialLocIds = preferredSecondarySpecialLocIds entity,
          referralCode = referralCode entity,
          referredByDriverId = referredByDriverId entity,
          referredByFleetOwnerId = referredByFleetOwnerId entity,
          referredByOperatorId = referredByOperatorId entity,
          rideRequestVolume = rideRequestVolume entity,
          rideRequestVolumeEnabled = rideRequestVolumeEnabled entity,
          ruleBasedUpgradeTiers = ruleBasedUpgradeTiers entity,
          servicesEnabledForSubscription = servicesEnabledForSubscription entity,
          softBlockExpiryTime = softBlockExpiryTime entity,
          softBlockReasonFlag = softBlockReasonFlag entity,
          softBlockStiers = softBlockStiers entity,
          specialLocWarriorEnabledAt = specialLocWarriorEnabledAt entity,
          subscribed = subscribed entity,
          tollRelatedIssueCount = tollRelatedIssueCount entity,
          tollRouteBlockedTill = tollRouteBlockedTill entity,
          totalReferred = totalReferred entity,
          tripDistanceMaxThreshold = tripDistanceMaxThreshold entity,
          tripDistanceMinThreshold = tripDistanceMinThreshold entity,
          verified = verified entity,
          weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown entity,
          weeklyExtraKms = weeklyExtraKms entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          createdAt = createdAt entity,
          updatedAt = updatedAt entity
        }
  decryptItem entity = do
    aadhaarNumber_ <- fmap fst <$> decryptItem (aadhaarNumber entity)
    dlNumber_ <- fmap fst <$> decryptItem (dlNumber entity)
    panNumber_ <- fmap fst <$> decryptItem (panNumber entity)
    pure
      ( DriverInformation
          { aadhaarNumber = aadhaarNumber_,
            aadhaarVerified = aadhaarVerified entity,
            acRestrictionLiftCount = acRestrictionLiftCount entity,
            acUsageRestrictionType = acUsageRestrictionType entity,
            active = active entity,
            address = address entity,
            addressDocumentType = addressDocumentType entity,
            adminId = adminId entity,
            airConditionScore = airConditionScore entity,
            approved = approved entity,
            autoPayStatus = autoPayStatus entity,
            availableUpiApps = availableUpiApps entity,
            blockExpiryTime = blockExpiryTime entity,
            blockReasonFlag = blockReasonFlag entity,
            blockStateModifier = blockStateModifier entity,
            blocked = blocked entity,
            blockedReason = blockedReason entity,
            canDowngradeToHatchback = canDowngradeToHatchback entity,
            canDowngradeToSedan = canDowngradeToSedan entity,
            canDowngradeToTaxi = canDowngradeToTaxi entity,
            canSwitchToInterCity = canSwitchToInterCity entity,
            canSwitchToIntraCity = canSwitchToIntraCity entity,
            canSwitchToRental = canSwitchToRental entity,
            compAadhaarImagePath = compAadhaarImagePath entity,
            dailyCancellationRateBlockingCooldown = dailyCancellationRateBlockingCooldown entity,
            dailyExtraKms = dailyExtraKms entity,
            dlNumber = dlNumber_,
            driverDob = driverDob entity,
            driverFlowStatus = driverFlowStatus entity,
            driverId = driverId entity,
            driverTripEndLocation = driverTripEndLocation entity,
            drunkAndDriveViolationCount = drunkAndDriveViolationCount entity,
            enabled = enabled entity,
            enabledAt = enabledAt entity,
            extraFareMitigationFlag = extraFareMitigationFlag entity,
            forwardBatchingEnabled = forwardBatchingEnabled entity,
            hasAdvanceBooking = hasAdvanceBooking entity,
            hasRideStarted = hasRideStarted entity,
            isBlockedForReferralPayout = isBlockedForReferralPayout entity,
            isHighAccuracyLocationEnabled = isHighAccuracyLocationEnabled entity,
            isInteroperable = isInteroperable entity,
            isPetModeEnabled = isPetModeEnabled entity,
            isSilentModeEnabled = isSilentModeEnabled entity,
            isSpecialLocWarrior = isSpecialLocWarrior entity,
            isTTSEnabled = isTTSEnabled entity,
            issueBreachCooldownTimes = issueBreachCooldownTimes entity,
            lastACStatusCheckedAt = lastACStatusCheckedAt entity,
            lastEnabledOn = lastEnabledOn entity,
            lastOfflineTime = lastOfflineTime entity,
            latestScheduledBooking = latestScheduledBooking entity,
            latestScheduledPickup = latestScheduledPickup entity,
            maxPickupRadius = maxPickupRadius entity,
            mode = mode entity,
            numOfLocks = numOfLocks entity,
            onRide = onRide entity,
            onRideTripCategory = onRideTripCategory entity,
            onboardingAs = onboardingAs entity,
            onboardingVehicleCategory = onboardingVehicleCategory entity,
            onlineDurationRefreshedAt = onlineDurationRefreshedAt entity,
            panNumber = panNumber_,
            payerVpa = payerVpa entity,
            paymentPending = paymentPending entity,
            payoutRegAmountRefunded = payoutRegAmountRefunded entity,
            payoutRegistrationOrderId = payoutRegistrationOrderId entity,
            payoutVpa = payoutVpa entity,
            payoutVpaBankAccount = payoutVpaBankAccount entity,
            payoutVpaStatus = payoutVpaStatus entity,
            preferredPrimarySpecialLocId = preferredPrimarySpecialLocId entity,
            preferredSecondarySpecialLocIds = preferredSecondarySpecialLocIds entity,
            referralCode = referralCode entity,
            referredByDriverId = referredByDriverId entity,
            referredByFleetOwnerId = referredByFleetOwnerId entity,
            referredByOperatorId = referredByOperatorId entity,
            rideRequestVolume = rideRequestVolume entity,
            rideRequestVolumeEnabled = rideRequestVolumeEnabled entity,
            ruleBasedUpgradeTiers = ruleBasedUpgradeTiers entity,
            servicesEnabledForSubscription = servicesEnabledForSubscription entity,
            softBlockExpiryTime = softBlockExpiryTime entity,
            softBlockReasonFlag = softBlockReasonFlag entity,
            softBlockStiers = softBlockStiers entity,
            specialLocWarriorEnabledAt = specialLocWarriorEnabledAt entity,
            subscribed = subscribed entity,
            tollRelatedIssueCount = tollRelatedIssueCount entity,
            tollRouteBlockedTill = tollRouteBlockedTill entity,
            totalReferred = totalReferred entity,
            tripDistanceMaxThreshold = tripDistanceMaxThreshold entity,
            tripDistanceMinThreshold = tripDistanceMinThreshold entity,
            verified = verified entity,
            weeklyCancellationRateBlockingCooldown = weeklyCancellationRateBlockingCooldown entity,
            weeklyExtraKms = weeklyExtraKms entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            createdAt = createdAt entity,
            updatedAt = updatedAt entity
          },
        ""
      )

instance EncryptedItem' DriverInformation where
  type UnencryptedItem DriverInformation = DecryptedDriverInformation
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data AddressDocumentType = RationCard | UtilityBill | Passport deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

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

data OnboardingAs = FLEET_DRIVER | INDIVIDUAL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PayoutVpaStatus = VIA_WEBHOOK | MANUALLY_ADDED | VERIFIED_BY_USER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AddressDocumentType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''AirConditionedRestrictionType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverAutoPayStatus)

$(mkHttpInstancesForEnum ''DriverAutoPayStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''OnboardingAs)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutVpaStatus)
