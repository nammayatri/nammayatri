{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Merchant where

import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Vector
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.Value
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data AllowedTripDistanceBoundsAPIEntity = AllowedTripDistanceBoundsAPIEntity
  { maxAllowedTripDistance :: Kernel.Types.Common.Meters,
    minAllowedTripDistance :: Kernel.Types.Common.Meters,
    maxAllowedTripDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    minAllowedTripDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BatchSplitByPickupDistance = BatchSplitByPickupDistance {batchSplitSize :: Kernel.Prelude.Int, batchSplitDelay :: Kernel.Types.Common.Seconds}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BatchSplitByPickupDistanceOnRide = BatchSplitByPickupDistanceOnRide {batchSplitSize :: Kernel.Prelude.Int, batchSplitDelay :: Kernel.Types.Common.Seconds}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ClearCacheSubscriptionReq = ClearCacheSubscriptionReq {serviceName :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.ServiceNames, planId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ClearCacheSubscriptionReq where
  hideSecrets = Kernel.Prelude.identity

data CongestionChargeMultiplierAPIEntity
  = BaseFareAndExtraDistanceFare Kernel.Types.Common.Centesimal
  | ExtraDistanceFare Kernel.Types.Common.Centesimal
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateFPDriverExtraFeeReq = CreateFPDriverExtraFeeReq
  { minFee :: Kernel.Types.Common.Money,
    maxFee :: Kernel.Types.Common.Money,
    stepFee :: Kernel.Types.Common.Money,
    defaultStepFee :: Kernel.Types.Common.Money,
    minFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    maxFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    stepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    defaultStepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateFPDriverExtraFeeReq where
  hideSecrets = Kernel.Prelude.identity

data DocumentType
  = RC
  | DL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DocumentVerificationConfigCreateReq = DocumentVerificationConfigCreateReq
  { checkExtraction :: Kernel.Prelude.Bool,
    checkExpiry :: Kernel.Prelude.Bool,
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    rcNumberPrefix :: Kernel.Prelude.Text,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    maxRetryCount :: Kernel.Prelude.Int,
    vehicleClassCheckType :: API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType,
    isDefaultEnabledOnManualVerification :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isImageValidationRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    doStrictVerifcation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    minFee :: Kernel.Types.Common.Money,
    maxFee :: Kernel.Types.Common.Money,
    stepFee :: Kernel.Types.Common.Money,
    defaultStepFee :: Kernel.Types.Common.Money,
    minFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    maxFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    stepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    defaultStepFeeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DocumentVerificationConfigCreateReq where
  hideSecrets = Kernel.Prelude.identity

data DocumentVerificationConfigItem = DocumentVerificationConfigItem
  { documentType :: API.Types.ProviderPlatform.Management.Merchant.DocumentType,
    checkExtraction :: Kernel.Prelude.Bool,
    checkExpiry :: Kernel.Prelude.Bool,
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    vehicleClassCheckType :: API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    maxRetryCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type DocumentVerificationConfigRes = [API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigItem]

data DocumentVerificationConfigUpdateReq = DocumentVerificationConfigUpdateReq
  { checkExtraction :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    checkExpiry :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    supportedVehicleClasses :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.SupportedVehicleClasses,
    rcNumberPrefix :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Text),
    rcNumberPrefixList :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [Kernel.Prelude.Text]),
    maxRetryCount :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    vehicleClassCheckType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue API.Types.ProviderPlatform.Management.Merchant.VehicleClassCheckType)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DocumentVerificationConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data DriverIntelligentPoolConfigRes = DriverIntelligentPoolConfigRes
  { availabilityTimeWeightage :: Kernel.Prelude.Int,
    availabilityTimeWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    acceptanceRatioWeightage :: Kernel.Prelude.Int,
    acceptanceRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: Kernel.Prelude.Int,
    cancellationAndRideFrequencyRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    intelligentPoolPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    speedNormalizer :: Kernel.Prelude.Double,
    driverSpeedWeightage :: Kernel.Prelude.Int,
    minLocationUpdates :: Kernel.Prelude.Int,
    locationUpdateSampleTime :: Kernel.Types.Common.Minutes,
    defaultDriverSpeed :: Kernel.Prelude.Double,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
  { availabilityTimeWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    availabilityTimeWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    acceptanceRatioWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    acceptanceRatioWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    cancellationAndRideFrequencyRatioWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    minQuotesToQualifyForIntelligentPoolWindowOption :: Kernel.Prelude.Maybe Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    intelligentPoolPercentage :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    speedNormalizer :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double),
    driverSpeedWeightage :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    minLocationUpdates :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    locationUpdateSampleTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Minutes),
    defaultDriverSpeed :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverIntelligentPoolConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data DriverPoolConfigCreateReq = DriverPoolConfigCreateReq
  { minRadiusOfSearch :: Kernel.Types.Common.Meters,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    radiusStepSize :: Kernel.Types.Common.Meters,
    minRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    maxRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    radiusStepSizeWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    onRideBatchSplitConfig :: [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistanceOnRide],
    onRideRadiusConfig :: [API.Types.ProviderPlatform.Management.Merchant.OnRideRadiusConfig],
    enableForwardBatching :: Kernel.Prelude.Bool,
    currentRideTripCategoryValidForForwardBatching :: [Kernel.Prelude.Text],
    batchSizeOnRide :: Kernel.Prelude.Int,
    batchSizeOnRideWithStraightLineDistance :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    useOneToOneOsrmMapping :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverBatchSize :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Int,
    poolSortingType :: API.Types.ProviderPlatform.Management.Merchant.PoolSortingType,
    distanceBasedBatchSplit :: [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistance],
    singleBatchProcessTime :: Kernel.Types.Common.Seconds,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    radiusShrinkValueForDriversOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    driverToDestinationDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    enableUnifiedPooling :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    dynamicBatchSize :: Data.Vector.Vector Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverPoolConfigCreateReq where
  hideSecrets = Kernel.Prelude.identity

data DriverPoolConfigItem = DriverPoolConfigItem
  { minRadiusOfSearch :: Kernel.Types.Common.Meters,
    maxRadiusOfSearch :: Kernel.Types.Common.Meters,
    radiusStepSize :: Kernel.Types.Common.Meters,
    minRadiusOfSearchWithUnit :: Kernel.Types.Common.Distance,
    maxRadiusOfSearchWithUnit :: Kernel.Types.Common.Distance,
    radiusStepSizeWithUnit :: Kernel.Types.Common.Distance,
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    actualDistanceThreshold :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    maxDriverQuotesRequired :: Kernel.Prelude.Int,
    driverQuoteLimit :: Kernel.Prelude.Int,
    driverRequestCountLimit :: Kernel.Prelude.Int,
    driverBatchSize :: Kernel.Prelude.Int,
    maxNumberOfBatches :: Kernel.Prelude.Int,
    maxParallelSearchRequests :: Kernel.Prelude.Int,
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Int,
    poolSortingType :: API.Types.ProviderPlatform.Management.Merchant.PoolSortingType,
    singleBatchProcessTime :: Kernel.Types.Common.Seconds,
    tripDistance :: Kernel.Types.Common.Meters,
    radiusShrinkValueForDriversOnRide :: Kernel.Types.Common.Meters,
    driverToDestinationDistanceThreshold :: Kernel.Types.Common.Meters,
    tripDistanceWithUnit :: Kernel.Types.Common.Distance,
    radiusShrinkValueForDriversOnRideWithUnit :: Kernel.Types.Common.Distance,
    driverToDestinationDistanceThresholdWithUnit :: Kernel.Types.Common.Distance,
    driverToDestinationDuration :: Kernel.Types.Common.Seconds,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type DriverPoolConfigRes = [API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigItem]

data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
  { minRadiusOfSearch :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    maxRadiusOfSearch :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    radiusStepSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    minRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    maxRadiusOfSearchWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    radiusStepSizeWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    driverPositionInfoExpiry :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Seconds),
    actualDistanceThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Meters),
    actualDistanceThresholdOnRide :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Meters),
    actualDistanceThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Distance),
    actualDistanceThresholdOnRideWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Distance),
    maxDriverQuotesRequired :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverQuoteLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverRequestCountLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxNumberOfBatches :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxParallelSearchRequests :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    maxParallelSearchRequestsOnRide :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    poolSortingType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue API.Types.ProviderPlatform.Management.Merchant.PoolSortingType),
    singleBatchProcessTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    distanceBasedBatchSplit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [API.Types.ProviderPlatform.Management.Merchant.BatchSplitByPickupDistance])
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data JobName
  = BadDebtCalculationTrigger
  | DriverFeeCalculationTrigger
  | SendManualPaymentLinkTrigger
  | ReferralPayoutTrigger
  | SupplyDemandCalculation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantCommonConfigRes = MerchantCommonConfigRes
  { pickupLocThreshold :: Kernel.Types.Common.Meters,
    dropLocThreshold :: Kernel.Types.Common.Meters,
    pickupLocThresholdWithUnit :: Kernel.Types.Common.Distance,
    dropLocThresholdWithUnit :: Kernel.Types.Common.Distance,
    rideTimeEstimatedThreshold :: Kernel.Types.Common.Seconds,
    includeDriverCurrentlyOnRide :: Kernel.Prelude.Bool,
    defaultPopupDelay :: Kernel.Types.Common.Seconds,
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    thresholdCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    minRidesForCancellationScore :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    mediaFileUrlPattern :: Kernel.Prelude.Text,
    mediaFileSizeUpperLimit :: Kernel.Prelude.Int,
    onboardingTryLimit :: Kernel.Prelude.Int,
    onboardingRetryTimeInHours :: Kernel.Prelude.Int,
    checkImageExtractionForDashboard :: Kernel.Prelude.Bool,
    searchRepeatLimit :: Kernel.Prelude.Int,
    actualRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    upwardsRecomputeBuffer :: Kernel.Types.Common.HighPrecMeters,
    upwardsRecomputeBufferPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    approxRideDistanceDiffThreshold :: Kernel.Types.Common.HighPrecMeters,
    actualRideDistanceDiffThresholdWithUnit :: Kernel.Types.Common.Distance,
    upwardsRecomputeBufferWithUnit :: Kernel.Types.Common.Distance,
    approxRideDistanceDiffThresholdWithUnit :: Kernel.Types.Common.Distance,
    minLocationAccuracy :: Kernel.Prelude.Double,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
  { pickupLocThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    dropLocThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Meters),
    pickupLocThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    dropLocThresholdWithUnit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Distance),
    rideTimeEstimatedThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    defaultPopupDelay :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    popupDelayToAddAsPenalty :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Types.Common.Seconds),
    thresholdCancellationScore :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    minRidesForCancellationScore :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    mediaFileUrlPattern :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Text),
    mediaFileSizeUpperLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    onboardingTryLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    onboardingRetryTimeInHours :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    checkImageExtractionForDashboard :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    searchRepeatLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverPaymentCycleBuffer :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentCycleDuration :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentCycleStartTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverPaymentReminderInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    timeDiffFromUtc :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    driverAutoPayNotificationTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverAutoPayExecutionTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverFeeMandateNotificationBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    driverFeeMandateExecutionBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    mandateNotificationRescheduleInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    mandateExecutionRescheduleInterval :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.NominalDiffTime),
    driverFeeCalculationTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    driverFeeCalculatorBatchSize :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.Int),
    driverFeeCalculatorBatchGap :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    orderAndNotificationStatusCheckTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    orderAndNotificationStatusCheckTimeLimit :: Kernel.Prelude.Maybe (Kernel.Types.Value.OptionalValue Kernel.Prelude.NominalDiffTime),
    snapToRoadConfidenceThreshold :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Double),
    useWithSnapToRoadFallback :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets MerchantCommonConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateRes = MerchantUpdateRes
  { name :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contactNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: API.Types.ProviderPlatform.Management.Merchant.Status,
    enabled :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NightShiftBounds = NightShiftBounds {nightShiftStart :: Kernel.Prelude.TimeOfDay, nightShiftEnd :: Kernel.Prelude.TimeOfDay}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NightShiftChargeAPIEntity
  = ProgressiveNightShiftCharge Kernel.Prelude.Float
  | ConstantNightShiftCharge Kernel.Types.Common.Money
  | ConstantNightShiftChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OnRideRadiusConfig = OnRideRadiusConfig {onRideRadius :: Kernel.Types.Common.Meters, onRideRadiusWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance, batchNumber :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PoolSortingType
  = Intelligent
  | Random
  | Tagged
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SchedulerTriggerReq = SchedulerTriggerReq
  { scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    jobName :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.JobName,
    jobData :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SchedulerTriggerReq where
  hideSecrets = Kernel.Prelude.identity

data Status
  = PENDING_VERIFICATION
  | APPROVED
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateFPPerExtraKmRateReq = UpdateFPPerExtraKmRateReq {perExtraKmRate :: Kernel.Types.Common.HighPrecMoney, perExtraKmRateWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFPPerExtraKmRateReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { serviceCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    serviceChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    nightShiftBounds :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.NightShiftBounds,
    allowedTripDistanceBounds :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.AllowedTripDistanceBoundsAPIEntity,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinuteRideExtraTimeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    perMinuteRideExtraTimeChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    congestionChargeMultiplier :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.CongestionChargeMultiplierAPIEntity,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    baseDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    baseDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    deadKmFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    baseFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    deadKmFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    waitingCharge :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.WaitingChargeAPIEntity,
    waitingChargeInfo :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.WaitingChargeInfoAPIEntity,
    freeWaitingTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    nightShiftCharge :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.NightShiftChargeAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFarePolicyReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpdateOnboardingVehicleVariantMappingReq = UpdateOnboardingVehicleVariantMappingReq {file :: Kernel.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateOnboardingVehicleVariantMappingReq where
  hideSecrets = Kernel.Prelude.identity

newtype UpsertFarePolicyReq = UpsertFarePolicyReq {file :: Kernel.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertFarePolicyReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertFarePolicyResp = UpsertFarePolicyResp {unprocessedFarePolicies :: [Kernel.Prelude.Text], success :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType
  = Infix
  | Prefix
  | Suffix
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WaitingChargeAPIEntity
  = PerMinuteWaitingCharge Kernel.Types.Common.HighPrecMoney
  | ConstantWaitingCharge Kernel.Types.Common.Money
  | PerMinuteWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  | ConstantWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WaitingChargeInfoAPIEntity = WaitingChargeInfoAPIEntity {freeWaitingTime :: Kernel.Types.Common.Minutes, waitingCharge :: API.Types.ProviderPlatform.Management.Merchant.WaitingChargeAPIEntity}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantConfigCommon :<|> PostMerchantConfigCommonUpdate :<|> GetMerchantConfigDriverPool :<|> PostMerchantConfigDriverPoolUpdate :<|> PostMerchantConfigDriverPoolCreate :<|> GetMerchantConfigDriverIntelligentPool :<|> PostMerchantConfigDriverIntelligentPoolUpdate :<|> GetMerchantConfigOnboardingDocument :<|> PostMerchantConfigOnboardingDocumentUpdate :<|> PostMerchantConfigOnboardingDocumentCreate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantServiceConfigVerificationUpdate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> PostMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> PostMerchantConfigFarePolicyUpdate :<|> PostMerchantConfigFarePolicyUpsert :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantSchedulerTrigger :<|> PostMerchantUpdateOnboardingVehicleVariantMapping :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigClearCacheSubscription :<|> PostMerchantConfigFailover))

type PostMerchantUpdate =
  ( "update" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes
  )

type GetMerchantConfigCommon = ("config" :> "common" :> Get '[JSON] API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes)

type PostMerchantConfigCommonUpdate =
  ( "config" :> "common" :> "update" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigDriverPool =
  ( "config" :> "driverPool" :> QueryParam "tripDistance" Kernel.Types.Common.Meters
      :> QueryParam
           "tripDistanceValue"
           Kernel.Types.Common.HighPrecDistance
      :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes
  )

type PostMerchantConfigDriverPoolUpdate =
  ( "config" :> "driverPool" :> "update" :> QueryParam "tripDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "distanceUnit"
           Kernel.Types.Common.DistanceUnit
      :> QueryParam "vehicleVariant" Dashboard.Common.VehicleVariant
      :> QueryParam
           "tripCategory"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "tripDistance"
           Kernel.Types.Common.Meters
      :> MandatoryQueryParam
           "area"
           Lib.Types.SpecialLocation.Area
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigDriverPoolCreate =
  ( "config" :> "driverPool" :> "create" :> QueryParam "tripDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "distanceUnit"
           Kernel.Types.Common.DistanceUnit
      :> QueryParam "vehiclevariant" Dashboard.Common.VehicleVariant
      :> QueryParam
           "tripCategory"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "tripDistance"
           Kernel.Types.Common.Meters
      :> MandatoryQueryParam
           "area"
           Lib.Types.SpecialLocation.Area
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigDriverIntelligentPool = ("config" :> "driverIntelligentPool" :> Get '[JSON] API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes)

type PostMerchantConfigDriverIntelligentPoolUpdate =
  ( "config" :> "driverIntelligentPool" :> "update"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigOnboardingDocument =
  ( "config" :> "onboardingDocument" :> QueryParam "documentType" API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> QueryParam
           "vehicleCategory"
           Dashboard.Common.VehicleCategory
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes
  )

type PostMerchantConfigOnboardingDocumentUpdate =
  ( "config" :> "onboardingDocument" :> "update"
      :> MandatoryQueryParam
           "documentType"
           API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> MandatoryQueryParam "category" Dashboard.Common.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigOnboardingDocumentCreate =
  ( "config" :> "onboardingDocument" :> "create"
      :> MandatoryQueryParam
           "documentType"
           API.Types.ProviderPlatform.Management.Merchant.DocumentType
      :> MandatoryQueryParam "category" Dashboard.Common.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantServiceUsageConfig = ("serviceUsageConfig" :> Get '[JSON] Dashboard.Common.Merchant.ServiceUsageConfigRes)

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigMapsUpdate =
  ( "serviceUsageConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceConfigSmsUpdate =
  ( "serviceConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceUsageConfigSmsUpdate =
  ( "serviceUsageConfig" :> "sms" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantServiceConfigVerificationUpdate =
  ( "serviceConfig" :> "verification" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.VerificationServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate =
  ( "config" :> "farePolicy"
      :> Capture
           "farePolicyId"
           (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> "driverExtraFeeBounds"
      :> "create"
      :> QueryParam "startDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "distanceUnit"
           Kernel.Types.Common.DistanceUnit
      :> MandatoryQueryParam
           "startDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate =
  ( "config" :> "farePolicy"
      :> Capture
           "farePolicyId"
           (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> "driverExtraFeeBounds"
      :> "update"
      :> QueryParam "startDistanceValue" Kernel.Types.Common.HighPrecDistance
      :> QueryParam
           "distanceUnit"
           Kernel.Types.Common.DistanceUnit
      :> MandatoryQueryParam
           "startDistance"
           Kernel.Types.Common.Meters
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyPerExtraKmRateUpdate =
  ( "config" :> "farePolicy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy)
      :> Capture
           "startDistance"
           Kernel.Types.Common.Meters
      :> "perExtraKmRate"
      :> "update"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyUpdate =
  ( "config" :> "farePolicy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy) :> "update"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyUpsert =
  ( "config" :> "farePolicy" :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq
      :> Post '[JSON] API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp
  )

type PostMerchantConfigOperatingCityCreate =
  ( "config" :> "operatingCity" :> "create"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.CreateMerchantOperatingCityReq
      :> Post '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigOperatingCityCreateHelper =
  ( "config" :> "operatingCity" :> "create" :> ReqBody '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantSchedulerTrigger =
  ( "scheduler" :> "trigger" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantUpdateOnboardingVehicleVariantMapping =
  ( "updateOnboardingVehicleVariantMapping"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationUpsert =
  ( "specialLocation" :> "upsert"
      :> QueryParam
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.Merchant.UpsertSpecialLocationReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationUpsertHelper =
  ( "specialLocation" :> "upsert" :> QueryParam "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.UpsertSpecialLocationReqT
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationDelete =
  ( "specialLocation" :> Capture "specialLocationId" (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsert =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpsertSpecialLocationGateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantSpecialLocationGatesUpsertHelper =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "upsert"
      :> ReqBody '[JSON] Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type DeleteMerchantSpecialLocationGatesDelete =
  ( "specialLocation"
      :> Capture
           "specialLocationId"
           (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation)
      :> "gates"
      :> "delete"
      :> Capture "gateName" Kernel.Prelude.Text
      :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigClearCacheSubscription =
  ( "config" :> "clearCache" :> "subscription"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.ClearCacheSubscriptionReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFailover =
  ( "config" :> Capture "configName" Dashboard.Common.Merchant.ConfigNames :> "failover"
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.ConfigFailoverReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes,
    getMerchantConfigCommon :: EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigRes,
    postMerchantConfigCommonUpdate :: API.Types.ProviderPlatform.Management.Merchant.MerchantCommonConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigDriverPool :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigRes,
    postMerchantConfigDriverPoolUpdate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigDriverPoolCreate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> API.Types.ProviderPlatform.Management.Merchant.DriverPoolConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigDriverIntelligentPool :: EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigRes,
    postMerchantConfigDriverIntelligentPoolUpdate :: API.Types.ProviderPlatform.Management.Merchant.DriverIntelligentPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigOnboardingDocument :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigRes,
    postMerchantConfigOnboardingDocumentUpdate :: API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOnboardingDocumentCreate :: API.Types.ProviderPlatform.Management.Merchant.DocumentType -> Dashboard.Common.VehicleCategory -> API.Types.ProviderPlatform.Management.Merchant.DocumentVerificationConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantServiceUsageConfig :: EulerHS.Types.EulerClient Dashboard.Common.Merchant.ServiceUsageConfigRes,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigVerificationUpdate :: Dashboard.Common.Merchant.VerificationServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyPerExtraKmRateUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> API.Types.ProviderPlatform.Management.Merchant.UpdateFPPerExtraKmRateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> API.Types.ProviderPlatform.Management.Merchant.UpdateFarePolicyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyUpsert ::
      ( Data.ByteString.Lazy.ByteString,
        API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyReq
      ) ->
      EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.UpsertFarePolicyResp,
    postMerchantConfigOperatingCityCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    postMerchantSchedulerTrigger :: API.Types.ProviderPlatform.Management.Merchant.SchedulerTriggerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantUpdateOnboardingVehicleVariantMapping ::
      ( Data.ByteString.Lazy.ByteString,
        API.Types.ProviderPlatform.Management.Merchant.UpdateOnboardingVehicleVariantMappingReq
      ) ->
      EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationGatesUpsert :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationGatesDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigClearCacheSubscription :: API.Types.ProviderPlatform.Management.Merchant.ClearCacheSubscriptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFailover :: Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> getMerchantConfigCommon :<|> postMerchantConfigCommonUpdate :<|> getMerchantConfigDriverPool :<|> postMerchantConfigDriverPoolUpdate :<|> postMerchantConfigDriverPoolCreate :<|> getMerchantConfigDriverIntelligentPool :<|> postMerchantConfigDriverIntelligentPoolUpdate :<|> getMerchantConfigOnboardingDocument :<|> postMerchantConfigOnboardingDocumentUpdate :<|> postMerchantConfigOnboardingDocumentCreate :<|> getMerchantServiceUsageConfig :<|> postMerchantServiceConfigMapsUpdate :<|> postMerchantServiceUsageConfigMapsUpdate :<|> postMerchantServiceConfigSmsUpdate :<|> postMerchantServiceUsageConfigSmsUpdate :<|> postMerchantServiceConfigVerificationUpdate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> postMerchantConfigFarePolicyUpdate :<|> postMerchantConfigFarePolicyUpsert :<|> postMerchantConfigOperatingCityCreate :<|> postMerchantSchedulerTrigger :<|> postMerchantUpdateOnboardingVehicleVariantMapping :<|> postMerchantSpecialLocationUpsert :<|> deleteMerchantSpecialLocationDelete :<|> postMerchantSpecialLocationGatesUpsert :<|> deleteMerchantSpecialLocationGatesDelete :<|> postMerchantConfigClearCacheSubscription :<|> postMerchantConfigFailover = merchantClient

data MerchantEndpointDSL
  = PostMerchantUpdateEndpoint
  | GetMerchantConfigCommonEndpoint
  | PostMerchantConfigCommonUpdateEndpoint
  | GetMerchantConfigDriverPoolEndpoint
  | PostMerchantConfigDriverPoolUpdateEndpoint
  | PostMerchantConfigDriverPoolCreateEndpoint
  | GetMerchantConfigDriverIntelligentPoolEndpoint
  | PostMerchantConfigDriverIntelligentPoolUpdateEndpoint
  | GetMerchantConfigOnboardingDocumentEndpoint
  | PostMerchantConfigOnboardingDocumentUpdateEndpoint
  | PostMerchantConfigOnboardingDocumentCreateEndpoint
  | GetMerchantServiceUsageConfigEndpoint
  | PostMerchantServiceConfigMapsUpdateEndpoint
  | PostMerchantServiceUsageConfigMapsUpdateEndpoint
  | PostMerchantServiceConfigSmsUpdateEndpoint
  | PostMerchantServiceUsageConfigSmsUpdateEndpoint
  | PostMerchantServiceConfigVerificationUpdateEndpoint
  | PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreateEndpoint
  | PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdateEndpoint
  | PostMerchantConfigFarePolicyPerExtraKmRateUpdateEndpoint
  | PostMerchantConfigFarePolicyUpdateEndpoint
  | PostMerchantConfigFarePolicyUpsertEndpoint
  | PostMerchantConfigOperatingCityCreateEndpoint
  | PostMerchantSchedulerTriggerEndpoint
  | PostMerchantUpdateOnboardingVehicleVariantMappingEndpoint
  | PostMerchantSpecialLocationUpsertEndpoint
  | DeleteMerchantSpecialLocationDeleteEndpoint
  | PostMerchantSpecialLocationGatesUpsertEndpoint
  | DeleteMerchantSpecialLocationGatesDeleteEndpoint
  | PostMerchantConfigClearCacheSubscriptionEndpoint
  | PostMerchantConfigFailoverEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
