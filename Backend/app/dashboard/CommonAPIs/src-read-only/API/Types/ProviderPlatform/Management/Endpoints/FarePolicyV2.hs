{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.FarePolicyV2 where

import qualified Dashboard.Common
import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.TimeBound
import Kernel.Utils.TH
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data FPV2AllowedTripDistanceBounds = FPV2AllowedTripDistanceBounds {minAllowedTripDistance :: Kernel.Types.Common.Meters, maxAllowedTripDistance :: Kernel.Types.Common.Meters}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2AmbulanceDetails = FPV2AmbulanceDetails {slabs :: [FPV2AmbulanceSlab]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2AmbulanceSlab = FPV2AmbulanceSlab
  { vehicleAge :: Kernel.Types.Time.Months,
    baseFare :: Kernel.Types.Common.HighPrecMoney,
    baseDistance :: Kernel.Types.Common.Meters,
    perKmRate :: Kernel.Types.Common.HighPrecMoney,
    waitingChargeInfo :: Kernel.Prelude.Maybe FPV2WaitingChargeInfo,
    platformFeeInfo :: Kernel.Prelude.Maybe FPV2PlatformFeeInfo,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2BoothCharge
  = BoothChargeFixed Kernel.Types.Common.HighPrecMoney
  | BoothChargePercentage Kernel.Prelude.Double
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2BulkReplaceItem = FPV2BulkReplaceItem {farePolicyId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy, issues :: [FPV2ValidationIssue], diff :: [FPV2FieldDiff]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2BulkReplaceReq = FPV2BulkReplaceReq {replacements :: [FPV2Replacement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2BulkReplaceReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2BulkReplaceRes = FPV2BulkReplaceRes {applied :: Kernel.Prelude.Bool, results :: [FPV2BulkReplaceItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2CancellationFarePolicy = FPV2CancellationFarePolicy
  { description :: Kernel.Prelude.Text,
    freeCancellationTimeSeconds :: Kernel.Types.Common.Seconds,
    maxWaitingTimeAtPickupSeconds :: Kernel.Types.Common.Seconds,
    minCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    maxCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMetreCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    perMinuteCancellationCharge :: Kernel.Types.Common.HighPrecMoney,
    percentageOfRideFareToBeCharged :: Kernel.Types.Common.Centesimal
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2CardCharge = FPV2CardCharge {perDistanceUnitMultiplier :: Kernel.Prelude.Maybe Kernel.Prelude.Double, fixed :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ChangeRequest = FPV2ChangeRequest
  { requestId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest,
    action :: Dashboard.Common.FPV2ChangeAction,
    status :: FPV2ChangeRequestStatus,
    fareProductId :: Kernel.Types.Id.Id Dashboard.Common.FareProduct,
    comboSummary :: Kernel.Prelude.Text,
    requestedBy :: Kernel.Prelude.Text,
    checkedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ChangeRequestListRes = FPV2ChangeRequestListRes {requests :: [FPV2ChangeRequest]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ChangeRequestRes = FPV2ChangeRequestRes {requestId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest, status :: FPV2ChangeRequestStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ChangeRequestStatus
  = PENDING
  | APPROVED
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data FPV2ConditionalCharge = FPV2ConditionalCharge
  { chargeCategory :: FPV2ConditionalChargeCategory,
    charge :: Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ConditionalChargeCategory
  = SAFETY_PLUS_CHARGES
  | NYREGULAR_SUBSCRIPTION_CHARGE
  | NO_CHARGES
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2CongestionChargeMultiplier
  = BaseFareAndExtraDistanceFare Kernel.Types.Common.Centesimal
  | ExtraDistanceFare Kernel.Types.Common.Centesimal
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2CreateProductReq = FPV2CreateProductReq
  { serviceTier :: Dashboard.Common.ServiceTierType,
    tripCategory :: Dashboard.Common.TripCategory,
    area :: Lib.Types.SpecialLocation.Area,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    searchSource :: FPV2SearchSource,
    enabled :: Kernel.Prelude.Bool,
    disableRecompute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    sourceFarePolicyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.FarePolicy),
    policy :: Kernel.Prelude.Maybe FPV2Policy
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2CreateProductReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2CreateProductRes = FPV2CreateProductRes {fareProductId :: Kernel.Types.Id.Id Dashboard.Common.FareProduct, farePolicyId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2DecideChangeRequestReq = FPV2DecideChangeRequestReq {approve :: Kernel.Prelude.Bool, remarks :: Kernel.Prelude.Maybe Kernel.Prelude.Text, checkedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2DecideChangeRequestReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2DriverExtraFeeBounds = FPV2DriverExtraFeeBounds
  { startDistance :: Kernel.Types.Common.Meters,
    stepFee :: Kernel.Types.Common.HighPrecMoney,
    defaultStepFee :: Kernel.Types.Common.HighPrecMoney,
    minFee :: Kernel.Types.Common.HighPrecMoney,
    maxFee :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FareBreakupItem = FPV2FareBreakupItem {title :: Kernel.Prelude.Text, amount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FareChargeComponent
  = RideFare
  | WaitingCharge
  | ServiceChargeComponent
  | TollChargesComponent
  | CongestionChargeComponent
  | ParkingChargeComponent
  | PetChargeComponent
  | PriorityChargeComponent
  | NightShiftChargeComponent
  | InsuranceChargeComponent
  | StopChargeComponent
  | LuggageChargeComponent
  | PlatformFeeComponent
  | CustomerCancellationChargeComponent
  | CustomerExtraFeeComponent
  | DeadKmFareComponent
  | ExtraKmFareComponent
  | RideDurationFareComponent
  | TimeBasedFareComponent
  | DistBasedFareComponent
  | TimeFareComponent
  | DistanceFareComponent
  | PickupChargeComponent
  | ExtraDistanceFareComponent
  | ExtraTimeFareComponent
  | StateEntryPermitChargesComponent
  | AmbulanceDistBasedFareComponent
  | RideVatComponent
  | TollVatComponent
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FareChargeConfig = FPV2FareChargeConfig {value :: Kernel.Prelude.Text, appliesOn :: [FPV2FareChargeComponent]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FarePolicyDetails
  = FPV2Progressive FPV2ProgressiveDetails
  | FPV2Slabs FPV2SlabsDetails
  | FPV2Rental FPV2RentalDetails
  | FPV2InterCity FPV2InterCityDetails
  | FPV2Ambulance FPV2AmbulanceDetails
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FarePolicyType
  = Progressive
  | Slabs
  | Rental
  | InterCity
  | Ambulance
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2FieldDiff = FPV2FieldDiff {field :: Kernel.Prelude.Text, oldValue :: Kernel.Prelude.Maybe Kernel.Prelude.Text, newValue :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2InterCityDetails = FPV2InterCityDetails
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    perHourCharge :: Kernel.Types.Common.HighPrecMoney,
    perKmRateOneWay :: Kernel.Types.Common.HighPrecMoney,
    perKmRateRoundTrip :: Kernel.Types.Common.HighPrecMoney,
    perExtraKmRate :: Kernel.Types.Common.HighPrecMoney,
    perExtraMinRate :: Kernel.Types.Common.HighPrecMoney,
    kmPerPlannedExtraHour :: Kernel.Types.Common.Kilometers,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    perDayMaxHourAllowance :: Kernel.Types.Common.Hours,
    perDayMaxAllowanceInMins :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    defaultWaitTimeAtDestination :: Kernel.Types.Common.Minutes,
    stateEntryPermitCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pricingSlabs :: [FPV2PricingSlab],
    waitingChargeInfo :: Kernel.Prelude.Maybe FPV2WaitingChargeInfo,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2NightShiftBounds = FPV2NightShiftBounds {nightShiftStart :: Kernel.Prelude.TimeOfDay, nightShiftEnd :: Kernel.Prelude.TimeOfDay}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2NightShiftCharge
  = ProgressiveNightShiftCharge Kernel.Prelude.Float
  | ConstantNightShiftCharge Kernel.Types.Common.HighPrecMoney
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PerExtraKmRateSection = FPV2PerExtraKmRateSection {startDistance :: Kernel.Types.Common.Meters, perExtraKmRate :: Kernel.Types.Common.HighPrecMoney, baseFareDepreciation :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PerMinRateDurationBasis
  = TotalDuration
  | TrafficDelayDuration
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PerMinRateSection = FPV2PerMinRateSection {rideDurationInMin :: Kernel.Prelude.Int, perMinRate :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PickupCharges = FPV2PickupCharges {pickupChargesMin :: Kernel.Types.Common.HighPrecMoney, pickupChargesMax :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PlatformFeeCharge
  = ProgressivePlatformFee Kernel.Types.Common.HighPrecMoney
  | ConstantPlatformFee Kernel.Types.Common.HighPrecMoney
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PlatformFeeInfo = FPV2PlatformFeeInfo {platformFeeCharge :: FPV2PlatformFeeCharge, cgst :: Kernel.Prelude.Double, sgst :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PlatformFeeMethod
  = Subscription
  | FixedAmount
  | None
  | SlabBased
  | NoCharge
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2Policy = FPV2Policy
  { farePolicyDetails :: FPV2FarePolicyDetails,
    driverExtraFeeBounds :: Kernel.Prelude.Maybe [FPV2DriverExtraFeeBounds],
    serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    parkingCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perStopCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perLuggageCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    returnFee :: Kernel.Prelude.Maybe FPV2ReturnFee,
    boothCharges :: Kernel.Prelude.Maybe FPV2BoothCharge,
    nightShiftBounds :: Kernel.Prelude.Maybe FPV2NightShiftBounds,
    allowedTripDistanceBounds :: Kernel.Prelude.Maybe FPV2AllowedTripDistanceBounds,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    petCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverAllowance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    airportConvenienceFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    businessDiscountPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    personalDiscountPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priorityCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    tipOptions :: Kernel.Prelude.Maybe [Kernel.Prelude.Int],
    additionalCongestionCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perMinuteRideExtraTimeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rideExtraTimeChargeGracePeriod :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    congestionChargeMultiplier :: Kernel.Prelude.Maybe FPV2CongestionChargeMultiplier,
    perDistanceUnitInsuranceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cardCharge :: Kernel.Prelude.Maybe FPV2CardCharge,
    vatChargeConfig :: Kernel.Prelude.Maybe FPV2FareChargeConfig,
    commissionChargeConfig :: Kernel.Prelude.Maybe FPV2FareChargeConfig,
    cancellationCommissionChargeConfig :: Kernel.Prelude.Maybe FPV2FareChargeConfig,
    tollTaxChargeConfig :: Kernel.Prelude.Maybe FPV2FareChargeConfig,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeCgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeSgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    platformFeeChargesBy :: Kernel.Prelude.Maybe FPV2PlatformFeeMethod,
    conditionalCharges :: Kernel.Prelude.Maybe [FPV2ConditionalCharge],
    driverCancellationNotAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    cancellationFarePolicy :: Kernel.Prelude.Maybe FPV2CancellationFarePolicy,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PolicyRes = FPV2PolicyRes
  { farePolicyId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    policy :: FPV2Policy
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PolicySummary = FPV2PolicySummary
  { farePolicyType :: FPV2FarePolicyType,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perKmRate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perMinRate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge,
    congestionChargeMultiplier :: Kernel.Prelude.Maybe FPV2CongestionChargeMultiplier
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PreviewReq = FPV2PreviewReq
  { farePolicyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.FarePolicy),
    policy :: Kernel.Prelude.Maybe FPV2Policy,
    serviceTier :: Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType,
    tripCategory :: Kernel.Prelude.Maybe Dashboard.Common.TripCategory,
    sampleTrips :: [FPV2SampleTrip]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2PreviewReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2PreviewRes = FPV2PreviewRes {trips :: [FPV2TripPreview]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2PricingSlab = FPV2PricingSlab
  { timePercentage :: Kernel.Prelude.Int,
    distancePercentage :: Kernel.Prelude.Int,
    farePercentage :: Kernel.Prelude.Int,
    includeActualTimePercentage :: Kernel.Prelude.Bool,
    includeActualDistPercentage :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ProductListItem = FPV2ProductListItem
  { fareProductId :: Kernel.Types.Id.Id Dashboard.Common.FareProduct,
    farePolicyId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy,
    serviceTier :: Dashboard.Common.ServiceTierType,
    tripCategory :: Dashboard.Common.TripCategory,
    area :: Lib.Types.SpecialLocation.Area,
    timeBounds :: Kernel.Types.TimeBound.TimeBound,
    searchSource :: FPV2SearchSource,
    enabled :: Kernel.Prelude.Bool,
    disableRecompute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    summary :: FPV2PolicySummary
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ProductListRes = FPV2ProductListRes {fareProducts :: [FPV2ProductListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ProgressiveDetails = FPV2ProgressiveDetails
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    baseDistance :: Kernel.Types.Common.Meters,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    pickupCharges :: FPV2PickupCharges,
    perExtraKmRateSections :: [FPV2PerExtraKmRateSection],
    perMinRateSections :: Kernel.Prelude.Maybe [FPV2PerMinRateSection],
    perMinRateDurationBasis :: Kernel.Prelude.Maybe FPV2PerMinRateDurationBasis,
    waitingChargeInfo :: Kernel.Prelude.Maybe FPV2WaitingChargeInfo,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2RemoveProductReq = FPV2RemoveProductReq {reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, requestedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2RemoveProductReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2RentalDetails = FPV2RentalDetails
  { baseFare :: Kernel.Types.Common.HighPrecMoney,
    perHourCharge :: Kernel.Types.Common.HighPrecMoney,
    perExtraKmRate :: Kernel.Types.Common.HighPrecMoney,
    perExtraMinRate :: Kernel.Types.Common.HighPrecMoney,
    includedKmPerHr :: Kernel.Types.Common.Kilometers,
    plannedPerKmRate :: Kernel.Types.Common.HighPrecMoney,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    maxAdditionalKmsLimit :: Kernel.Types.Common.Kilometers,
    totalAdditionalKmsLimit :: Kernel.Types.Common.Kilometers,
    distanceBuffers :: [FPV2RentalDistanceBuffer],
    pricingSlabs :: [FPV2PricingSlab],
    waitingChargeInfo :: Kernel.Prelude.Maybe FPV2WaitingChargeInfo,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2RentalDistanceBuffer = FPV2RentalDistanceBuffer {rideDuration :: Kernel.Types.Common.Seconds, bufferKms :: Kernel.Prelude.Int, bufferMeters :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ReplaceReq = FPV2ReplaceReq {policy :: FPV2Policy}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2ReplaceReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2ReplaceRes = FPV2ReplaceRes {applied :: Kernel.Prelude.Bool, issues :: [FPV2ValidationIssue], diff :: [FPV2FieldDiff]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2Replacement = FPV2Replacement {farePolicyId :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy, policy :: FPV2Policy}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2ReturnFee
  = ReturnFeeFixed Kernel.Types.Common.HighPrecMoney
  | ReturnFeePercentage Kernel.Prelude.Double
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2SampleTrip = FPV2SampleTrip
  { distance :: Kernel.Types.Common.Meters,
    duration :: Kernel.Types.Common.Seconds,
    rideTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    waitingTimeMin :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2SearchSource
  = ALL
  | DASHBOARD
  | MOBILE_APP
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2Slab = FPV2Slab
  { startDistance :: Kernel.Types.Common.Meters,
    baseFare :: Kernel.Types.Common.HighPrecMoney,
    waitingChargeInfo :: Kernel.Prelude.Maybe FPV2WaitingChargeInfo,
    platformFeeInfo :: Kernel.Prelude.Maybe FPV2PlatformFeeInfo,
    nightShiftCharge :: Kernel.Prelude.Maybe FPV2NightShiftCharge
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2SlabsDetails = FPV2SlabsDetails {slabs :: [FPV2Slab]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2Subscription = FPV2Subscription {email :: Kernel.Prelude.Text, alertType :: Dashboard.Common.FPV2AlertType, subscribedBy :: Kernel.Prelude.Text, createdAt :: Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2SubscriptionListRes = FPV2SubscriptionListRes {subscriptions :: [FPV2Subscription]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2SubscriptionReq = FPV2SubscriptionReq {email :: Kernel.Prelude.Text, alertType :: Dashboard.Common.FPV2AlertType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2SubscriptionReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2TripPreview = FPV2TripPreview
  { distance :: Kernel.Types.Common.Meters,
    duration :: Kernel.Types.Common.Seconds,
    totalFare :: Kernel.Types.Common.HighPrecMoney,
    minFare :: Kernel.Types.Common.HighPrecMoney,
    maxFare :: Kernel.Types.Common.HighPrecMoney,
    breakup :: [FPV2FareBreakupItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2UpdateProductReq = FPV2UpdateProductReq
  { enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    disableRecompute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    timeBounds :: Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound,
    detachFarePolicy :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets FPV2UpdateProductReq where
  hideSecrets = Kernel.Prelude.identity

data FPV2ValidationIssue = FPV2ValidationIssue {field :: Kernel.Prelude.Text, message :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2WaitingCharge
  = PerMinuteWaitingCharge Kernel.Types.Common.HighPrecMoney
  | ConstantWaitingCharge Kernel.Types.Common.HighPrecMoney
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FPV2WaitingChargeInfo = FPV2WaitingChargeInfo {freeWaitingTime :: Kernel.Types.Common.Minutes, waitingCharge :: FPV2WaitingCharge}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("farePolicyV2" :> (GetFarePolicyV2List :<|> GetFarePolicyV2Policy :<|> PostFarePolicyV2PolicyReplace :<|> PostFarePolicyV2BulkReplace :<|> PostFarePolicyV2Preview :<|> PostFarePolicyV2ProductCreate :<|> PostFarePolicyV2ProductUpdate :<|> PostFarePolicyV2ProductRemove :<|> GetFarePolicyV2ChangeRequestList :<|> PostFarePolicyV2ChangeRequestDecide :<|> GetFarePolicyV2AlertsSubscriptions :<|> PostFarePolicyV2AlertsSubscribe :<|> PostFarePolicyV2AlertsUnsubscribe))

type GetFarePolicyV2List =
  ( "list" :> QueryParam "tripCategory" Dashboard.Common.TripCategory :> QueryParam "area" Lib.Types.SpecialLocation.Area
      :> QueryParam
           "serviceTier"
           Dashboard.Common.ServiceTierType
      :> QueryParam "enabled" Kernel.Prelude.Bool
      :> Get '[JSON] FPV2ProductListRes
  )

type GetFarePolicyV2Policy = ("policy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy) :> Get '[JSON] FPV2PolicyRes)

type PostFarePolicyV2PolicyReplace =
  ( "policy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy) :> "replace" :> QueryParam "dryRun" Kernel.Prelude.Bool
      :> ReqBody
           '[JSON]
           FPV2ReplaceReq
      :> Post '[JSON] FPV2ReplaceRes
  )

type PostFarePolicyV2BulkReplace = ("bulkReplace" :> QueryParam "dryRun" Kernel.Prelude.Bool :> ReqBody '[JSON] FPV2BulkReplaceReq :> Post '[JSON] FPV2BulkReplaceRes)

type PostFarePolicyV2Preview = ("preview" :> ReqBody '[JSON] FPV2PreviewReq :> Post '[JSON] FPV2PreviewRes)

type PostFarePolicyV2ProductCreate = ("product" :> "create" :> ReqBody '[JSON] FPV2CreateProductReq :> Post '[JSON] FPV2CreateProductRes)

type PostFarePolicyV2ProductUpdate =
  ( "product" :> Capture "fareProductId" (Kernel.Types.Id.Id Dashboard.Common.FareProduct) :> "update" :> ReqBody '[JSON] FPV2UpdateProductReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFarePolicyV2ProductRemove =
  ( "product" :> Capture "fareProductId" (Kernel.Types.Id.Id Dashboard.Common.FareProduct) :> "remove" :> ReqBody '[JSON] FPV2RemoveProductReq
      :> Post
           '[JSON]
           FPV2ChangeRequestRes
  )

type GetFarePolicyV2ChangeRequestList = ("changeRequest" :> "list" :> QueryParam "status" FPV2ChangeRequestStatus :> Get '[JSON] FPV2ChangeRequestListRes)

type PostFarePolicyV2ChangeRequestDecide =
  ( "changeRequest" :> Capture "requestId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest) :> "decide"
      :> ReqBody
           '[JSON]
           FPV2DecideChangeRequestReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetFarePolicyV2AlertsSubscriptions = ("alerts" :> "subscriptions" :> Get '[JSON] FPV2SubscriptionListRes)

type PostFarePolicyV2AlertsSubscribe = ("alerts" :> "subscribe" :> ReqBody '[JSON] FPV2SubscriptionReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFarePolicyV2AlertsUnsubscribe = ("alerts" :> "unsubscribe" :> ReqBody '[JSON] FPV2SubscriptionReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data FarePolicyV2APIs = FarePolicyV2APIs
  { getFarePolicyV2List :: Kernel.Prelude.Maybe Dashboard.Common.TripCategory -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area -> Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient FPV2ProductListRes,
    getFarePolicyV2Policy :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> EulerHS.Types.EulerClient FPV2PolicyRes,
    postFarePolicyV2PolicyReplace :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> FPV2ReplaceReq -> EulerHS.Types.EulerClient FPV2ReplaceRes,
    postFarePolicyV2BulkReplace :: Kernel.Prelude.Maybe Kernel.Prelude.Bool -> FPV2BulkReplaceReq -> EulerHS.Types.EulerClient FPV2BulkReplaceRes,
    postFarePolicyV2Preview :: FPV2PreviewReq -> EulerHS.Types.EulerClient FPV2PreviewRes,
    postFarePolicyV2ProductCreate :: FPV2CreateProductReq -> EulerHS.Types.EulerClient FPV2CreateProductRes,
    postFarePolicyV2ProductUpdate :: Kernel.Types.Id.Id Dashboard.Common.FareProduct -> FPV2UpdateProductReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFarePolicyV2ProductRemove :: Kernel.Types.Id.Id Dashboard.Common.FareProduct -> FPV2RemoveProductReq -> EulerHS.Types.EulerClient FPV2ChangeRequestRes,
    getFarePolicyV2ChangeRequestList :: Kernel.Prelude.Maybe FPV2ChangeRequestStatus -> EulerHS.Types.EulerClient FPV2ChangeRequestListRes,
    postFarePolicyV2ChangeRequestDecide :: Kernel.Types.Id.Id Dashboard.Common.FarePolicyChangeRequest -> FPV2DecideChangeRequestReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getFarePolicyV2AlertsSubscriptions :: EulerHS.Types.EulerClient FPV2SubscriptionListRes,
    postFarePolicyV2AlertsSubscribe :: FPV2SubscriptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFarePolicyV2AlertsUnsubscribe :: FPV2SubscriptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFarePolicyV2APIs :: (Client EulerHS.Types.EulerClient API -> FarePolicyV2APIs)
mkFarePolicyV2APIs farePolicyV2Client = (FarePolicyV2APIs {..})
  where
    getFarePolicyV2List :<|> getFarePolicyV2Policy :<|> postFarePolicyV2PolicyReplace :<|> postFarePolicyV2BulkReplace :<|> postFarePolicyV2Preview :<|> postFarePolicyV2ProductCreate :<|> postFarePolicyV2ProductUpdate :<|> postFarePolicyV2ProductRemove :<|> getFarePolicyV2ChangeRequestList :<|> postFarePolicyV2ChangeRequestDecide :<|> getFarePolicyV2AlertsSubscriptions :<|> postFarePolicyV2AlertsSubscribe :<|> postFarePolicyV2AlertsUnsubscribe = farePolicyV2Client

data FarePolicyV2UserActionType
  = GET_FARE_POLICY_V2_LIST
  | GET_FARE_POLICY_V2_POLICY
  | POST_FARE_POLICY_V2_POLICY_REPLACE
  | POST_FARE_POLICY_V2_BULK_REPLACE
  | POST_FARE_POLICY_V2_PREVIEW
  | POST_FARE_POLICY_V2_PRODUCT_CREATE
  | POST_FARE_POLICY_V2_PRODUCT_UPDATE
  | POST_FARE_POLICY_V2_PRODUCT_REMOVE
  | GET_FARE_POLICY_V2_CHANGE_REQUEST_LIST
  | POST_FARE_POLICY_V2_CHANGE_REQUEST_DECIDE
  | GET_FARE_POLICY_V2_ALERTS_SUBSCRIPTIONS
  | POST_FARE_POLICY_V2_ALERTS_SUBSCRIBE
  | POST_FARE_POLICY_V2_ALERTS_UNSUBSCRIBE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''FPV2ChangeRequestStatus)

$(Data.Singletons.TH.genSingletons [''FarePolicyV2UserActionType])
