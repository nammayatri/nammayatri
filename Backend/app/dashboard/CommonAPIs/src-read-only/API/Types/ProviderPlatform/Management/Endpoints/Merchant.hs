{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Merchant where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified Dashboard.Common
import qualified Dashboard.Common.Merchant
import Data.Aeson
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Vector
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters
import qualified Kernel.Types.Value
import Kernel.Utils.TH
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Types.SpecialLocation
import Servant
import Servant.Client

data Action
  = UPDATE_CONFIG
  | CREATE_CONFIG
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

data ClearCacheSubscriptionReq = ClearCacheSubscriptionReq
  { serviceName :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.ServiceNames,
    planId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tableName :: Kernel.Prelude.Maybe TableName
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ClearCacheSubscriptionReq where
  hideSecrets = Kernel.Prelude.identity

data Command
  = COMMIT
  | VIEW_DIFF
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

data DocumentFlowGrouping
  = COMMON
  | STANDARD
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentType
  = RC
  | DL
  | Permissions
  | SubscriptionPlan
  | ProfilePhoto
  | AadhaarCard
  | PanCard
  | VehiclePermit
  | VehicleFitnessCertificate
  | VehicleInsurance
  | VehiclePUC
  | ProfileDetails
  | SocialSecurityNumber
  | VehicleInspectionForm
  | UploadProfile
  | GSTCertificate
  | BackgroundVerification
  | VehicleFront
  | VehicleBack
  | VehicleRight
  | VehicleLeft
  | VehicleFrontInterior
  | VehicleBackInterior
  | VehicleNOC
  | BusinessLicense
  | Odometer
  | InspectionHub
  | KIWADriverCard
  | KIWATaxiPermit
  | KvKChamberOfCommerceRegistration
  | TAXDetails
  | BankingDetails
  | VehicleDetails
  | SchipolAirportAgreement
  | SchipolSmartcardProof
  | TXQualityMark
  | TaxiDriverPermit
  | TaxiTransportLicense
  | FinnishIDResidencePermit
  | BusinessRegistrationExtract
  | PersonalId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DocumentVerificationConfigCreateReq = DocumentVerificationConfigCreateReq
  { checkExtraction :: Kernel.Prelude.Bool,
    checkExpiry :: Kernel.Prelude.Bool,
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    rcNumberPrefix :: Kernel.Prelude.Text,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    maxRetryCount :: Kernel.Prelude.Int,
    vehicleClassCheckType :: VehicleClassCheckType,
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
    filterForOldApks :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    documentCategory :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentCategory,
    documentFlowGrouping :: Kernel.Prelude.Maybe DocumentFlowGrouping
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DocumentVerificationConfigCreateReq where
  hideSecrets = Kernel.Prelude.identity

data DocumentVerificationConfigItem = DocumentVerificationConfigItem
  { documentType :: DocumentType,
    checkExtraction :: Kernel.Prelude.Bool,
    checkExpiry :: Kernel.Prelude.Bool,
    supportedVehicleClasses :: Dashboard.Common.Merchant.SupportedVehicleClasses,
    vehicleClassCheckType :: VehicleClassCheckType,
    documentFlowGrouping :: DocumentFlowGrouping,
    rcNumberPrefixList :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    maxRetryCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type DocumentVerificationConfigRes = [DocumentVerificationConfigItem]

data DocumentVerificationConfigUpdateReq = DocumentVerificationConfigUpdateReq
  { checkExtraction :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    checkExpiry :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Bool),
    supportedVehicleClasses :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.SupportedVehicleClasses,
    rcNumberPrefix :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Text),
    rcNumberPrefixList :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [Kernel.Prelude.Text]),
    maxRetryCount :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Prelude.Int),
    vehicleClassCheckType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue VehicleClassCheckType),
    documentFlowGrouping :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue DocumentFlowGrouping)
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
    onRideBatchSplitConfig :: [BatchSplitByPickupDistanceOnRide],
    onRideRadiusConfig :: [OnRideRadiusConfig],
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
    poolSortingType :: PoolSortingType,
    distanceBasedBatchSplit :: [BatchSplitByPickupDistance],
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
    poolSortingType :: PoolSortingType,
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

type DriverPoolConfigRes = [DriverPoolConfigItem]

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
    poolSortingType :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue PoolSortingType),
    singleBatchProcessTime :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue Kernel.Types.Common.Seconds),
    distanceBasedBatchSplit :: Kernel.Prelude.Maybe (Kernel.Types.Value.MandatoryValue [BatchSplitByPickupDistance])
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data GeometryAPIEntity = GeometryAPIEntity {region :: Kernel.Prelude.Text, state :: Kernel.Types.Beckn.Context.IndianState, city :: Kernel.Types.Beckn.Context.City, geom :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type GeometryResp = [GeometryAPIEntity]

data JobName
  = BadDebtCalculationTrigger
  | DriverFeeCalculationTrigger
  | SendManualPaymentLinkTrigger
  | ReferralPayoutTrigger
  | SupplyDemandCalculation
  | CongestionChargeCalculation
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
    status :: Status,
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

data PayoutConfigReq = PayoutConfigReq
  { toEnable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    referralRewardAmountPerRide :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    vehicleCategory :: Dashboard.Common.VehicleCategory,
    payoutRegistrationFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationCgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutRegistrationSgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    thresholdPayoutAmountPerPerson :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PayoutConfigReq where
  hideSecrets = Kernel.Prelude.identity

data PoolSortingType
  = Intelligent
  | Random
  | Tagged
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SchedulerTriggerReq = SchedulerTriggerReq {scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, jobName :: Kernel.Prelude.Maybe JobName, jobData :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SchedulerTriggerReq where
  hideSecrets = Kernel.Prelude.identity

type SpecialLocationResp = [Lib.Queries.SpecialLocation.SpecialLocationFull]

data Status
  = PENDING_VERIFICATION
  | APPROVED
  | REJECTED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TableName
  = SUBSCRIPTION_CONFIG
  | PLAN
  | PLAN_TRANSLATION
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
    nightShiftBounds :: Kernel.Prelude.Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Kernel.Prelude.Maybe AllowedTripDistanceBoundsAPIEntity,
    govtCharges :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    perMinuteRideExtraTimeCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    priorityCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    pickupBufferInSecsForNightShiftCal :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    perMinuteRideExtraTimeChargeWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    congestionChargeMultiplier :: Kernel.Prelude.Maybe CongestionChargeMultiplierAPIEntity,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    baseDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    baseDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    deadKmFare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    baseFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    deadKmFareWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    waitingCharge :: Kernel.Prelude.Maybe WaitingChargeAPIEntity,
    waitingChargeInfo :: Kernel.Prelude.Maybe WaitingChargeInfoAPIEntity,
    freeWaitingTime :: Kernel.Prelude.Maybe Kernel.Types.Common.Minutes,
    nightShiftCharge :: Kernel.Prelude.Maybe NightShiftChargeAPIEntity,
    petCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    businessDiscountPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateFarePolicyReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateOnboardingVehicleVariantMappingReq = UpdateOnboardingVehicleVariantMappingReq {file :: Kernel.Prelude.FilePath, vehicleCategory :: Kernel.Prelude.Text}
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

data UpsertPlanAndConfigReq = UpsertPlanAndConfigReq {action :: Action, command :: Command, config :: Data.Aeson.Value, tableName :: TableName}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertPlanAndConfigReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertPlanAndConfigResp = UpsertPlanAndConfigResp
  { respCode :: Kernel.Types.APISuccess.APISuccess,
    executedAction :: Action,
    executedCommand :: Command,
    error_resp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    diff :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType
  = Infix
  | Prefix
  | Suffix
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleServiceTierConfigUpdateReq = VehicleServiceTierConfigUpdateReq
  { name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    airConditionedThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isIntercityEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isRentalsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowedVehicleVariant :: Kernel.Prelude.Maybe [Dashboard.Common.VehicleVariant],
    autoSelectedVehicleVariant :: Kernel.Prelude.Maybe [Dashboard.Common.VehicleVariant],
    defaultForVehicleVariant :: Kernel.Prelude.Maybe [Dashboard.Common.VehicleVariant],
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priority :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    baseVehicleServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    fareAdditionPerKmOverBaseServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopFcmThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopFcmSuppressCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    scheduleBookingListEligibilityTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets VehicleServiceTierConfigUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleServiceTierItem = VehicleServiceTierItem
  { serviceTierType :: Dashboard.Common.ServiceTierType,
    name :: Kernel.Prelude.Text,
    shortDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    longDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    seatingCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    airConditionedThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    isAirConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isIntercityEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isRentalsEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    allowedVehicleVariant :: [Dashboard.Common.VehicleVariant],
    autoSelectedVehicleVariant :: [Dashboard.Common.VehicleVariant],
    defaultForVehicleVariant :: [Dashboard.Common.VehicleVariant],
    vehicleCategory :: Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory,
    vehicleIconUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    priority :: Kernel.Prelude.Int,
    baseVehicleServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    fareAdditionPerKmOverBaseServiceTier :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    oxygen :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    ventilator :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    luggageCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopFcmThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopFcmSuppressCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    scheduleBookingListEligibilityTags :: Kernel.Prelude.Maybe [Kernel.Prelude.Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type VehicleServiceTierRes = [VehicleServiceTierItem]

data WaitingChargeAPIEntity
  = PerMinuteWaitingCharge Kernel.Types.Common.HighPrecMoney
  | ConstantWaitingCharge Kernel.Types.Common.Money
  | PerMinuteWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  | ConstantWaitingChargeWithCurrency Kernel.Types.Common.PriceAPIEntity
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WaitingChargeInfoAPIEntity = WaitingChargeInfoAPIEntity {freeWaitingTime :: Kernel.Types.Common.Minutes, waitingCharge :: WaitingChargeAPIEntity}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> GetMerchantConfigCommon :<|> PostMerchantConfigCommonUpdate :<|> GetMerchantConfigDriverPool :<|> PostMerchantConfigDriverPoolUpdate :<|> PostMerchantConfigDriverPoolCreate :<|> GetMerchantConfigDriverIntelligentPool :<|> PostMerchantConfigDriverIntelligentPoolUpdate :<|> GetMerchantConfigOnboardingDocument :<|> PostMerchantConfigOnboardingDocumentUpdate :<|> PostMerchantConfigOnboardingDocumentCreate :<|> GetMerchantServiceUsageConfig :<|> PostMerchantServiceConfigMapsUpdate :<|> PostMerchantServiceUsageConfigMapsUpdate :<|> PostMerchantServiceConfigSmsUpdate :<|> PostMerchantServiceUsageConfigSmsUpdate :<|> PostMerchantServiceConfigVerificationUpdate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> PostMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> PostMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> PostMerchantConfigFarePolicyUpdate :<|> PostMerchantConfigFarePolicyUpsert :<|> GetMerchantConfigFarePolicyExport :<|> PostMerchantConfigOperatingCityCreateHelper :<|> PostMerchantSchedulerTrigger :<|> PostMerchantUpdateOnboardingVehicleVariantMapping :<|> PostMerchantConfigSpecialLocationUpsert :<|> GetMerchantConfigSpecialLocationList :<|> GetMerchantConfigGeometryList :<|> PutMerchantConfigGeometryUpdate :<|> PostMerchantSpecialLocationUpsertHelper :<|> DeleteMerchantSpecialLocationDelete :<|> PostMerchantSpecialLocationGatesUpsertHelper :<|> DeleteMerchantSpecialLocationGatesDelete :<|> PostMerchantConfigClearCacheSubscription :<|> PostMerchantConfigUpsertPlanAndConfigSubscription :<|> PostMerchantConfigFailover :<|> PostMerchantPayoutConfigUpdate :<|> PostMerchantConfigOperatingCityWhiteList :<|> PostMerchantConfigMerchantCreateHelper :<|> GetMerchantConfigVehicleServiceTier :<|> PostMerchantConfigVehicleServiceTierUpdate))

type PostMerchantUpdate = ("update" :> ReqBody '[JSON] MerchantUpdateReq :> Post '[JSON] MerchantUpdateRes)

type GetMerchantConfigCommon = ("config" :> "common" :> Get '[JSON] MerchantCommonConfigRes)

type PostMerchantConfigCommonUpdate = ("config" :> "common" :> "update" :> ReqBody '[JSON] MerchantCommonConfigUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetMerchantConfigDriverPool =
  ( "config" :> "driverPool" :> QueryParam "tripDistance" Kernel.Types.Common.Meters
      :> QueryParam
           "tripDistanceValue"
           Kernel.Types.Common.HighPrecDistance
      :> QueryParam "distanceUnit" Kernel.Types.Common.DistanceUnit
      :> Get '[JSON] DriverPoolConfigRes
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
           DriverPoolConfigUpdateReq
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
           DriverPoolConfigCreateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigDriverIntelligentPool = ("config" :> "driverIntelligentPool" :> Get '[JSON] DriverIntelligentPoolConfigRes)

type PostMerchantConfigDriverIntelligentPoolUpdate =
  ( "config" :> "driverIntelligentPool" :> "update" :> ReqBody '[JSON] DriverIntelligentPoolConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetMerchantConfigOnboardingDocument =
  ( "config" :> "onboardingDocument" :> QueryParam "documentType" DocumentType :> QueryParam "vehicleCategory" Dashboard.Common.VehicleCategory
      :> Get
           '[JSON]
           DocumentVerificationConfigRes
  )

type PostMerchantConfigOnboardingDocumentUpdate =
  ( "config" :> "onboardingDocument" :> "update" :> MandatoryQueryParam "documentType" DocumentType
      :> MandatoryQueryParam
           "category"
           Dashboard.Common.VehicleCategory
      :> ReqBody '[JSON] DocumentVerificationConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigOnboardingDocumentCreate =
  ( "config" :> "onboardingDocument" :> "create" :> MandatoryQueryParam "documentType" DocumentType
      :> MandatoryQueryParam
           "category"
           Dashboard.Common.VehicleCategory
      :> ReqBody '[JSON] DocumentVerificationConfigCreateReq
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
           CreateFPDriverExtraFeeReq
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
           CreateFPDriverExtraFeeReq
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
      :> ReqBody '[JSON] UpdateFPPerExtraKmRateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyUpdate =
  ( "config" :> "farePolicy" :> Capture "farePolicyId" (Kernel.Types.Id.Id Dashboard.Common.FarePolicy) :> "update"
      :> ReqBody
           '[JSON]
           UpdateFarePolicyReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigFarePolicyUpsert =
  ( "config" :> "farePolicy" :> "upsert" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp UpsertFarePolicyReq
      :> Post
           '[JSON]
           UpsertFarePolicyResp
  )

type GetMerchantConfigFarePolicyExport = ("config" :> "farePolicy" :> "export" :> Get '[JSON] Kernel.Prelude.Text)

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

type PostMerchantSchedulerTrigger = ("scheduler" :> "trigger" :> ReqBody '[JSON] SchedulerTriggerReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantUpdateOnboardingVehicleVariantMapping =
  ( "updateOnboardingVehicleVariantMapping"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           UpdateOnboardingVehicleVariantMappingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantConfigSpecialLocationUpsert =
  ( "config" :> "specialLocation" :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq
      :> Post '[JSON] Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities
  )

type GetMerchantConfigSpecialLocationList =
  ( "config" :> "specialLocation" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "locationType"
           Lib.Types.SpecialLocation.SpecialLocationType
      :> Get '[JSON] SpecialLocationResp
  )

type GetMerchantConfigGeometryList = ("config" :> "geometry" :> "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get '[JSON] GeometryResp)

type PutMerchantConfigGeometryUpdate =
  ( "config" :> "geometry" :> "update"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.UpdateGeometryReq
      :> Put '[JSON] Kernel.Types.APISuccess.APISuccess
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

type PostMerchantConfigClearCacheSubscription = ("config" :> "clearCache" :> "subscription" :> ReqBody '[JSON] ClearCacheSubscriptionReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantConfigUpsertPlanAndConfigSubscription = ("config" :> "upsertPlanAndConfig" :> "subscription" :> ReqBody '[JSON] UpsertPlanAndConfigReq :> Post '[JSON] UpsertPlanAndConfigResp)

type PostMerchantConfigFailover =
  ( "config" :> Capture "configName" Dashboard.Common.Merchant.ConfigNames :> "failover"
      :> ReqBody
           '[JSON]
           Dashboard.Common.Merchant.ConfigFailoverReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostMerchantPayoutConfigUpdate = ("payoutConfig" :> "update" :> ReqBody '[JSON] PayoutConfigReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantConfigOperatingCityWhiteList =
  ( "config" :> "operatingCity" :> "whiteList" :> ReqBody '[JSON] Dashboard.Common.Merchant.WhiteListOperatingCityReq
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.WhiteListOperatingCityRes
  )

type PostMerchantConfigMerchantCreate =
  ( "config" :> "merchant" :> "create"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Merchant.CreateMerchantOperatingCityReq
      :> Post '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type PostMerchantConfigMerchantCreateHelper =
  ( "config" :> "merchant" :> "create" :> ReqBody '[JSON] Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT
      :> Post
           '[JSON]
           Dashboard.Common.Merchant.CreateMerchantOperatingCityRes
  )

type GetMerchantConfigVehicleServiceTier = ("config" :> "vehicleServiceTier" :> QueryParam "serviceTierType" Dashboard.Common.ServiceTierType :> Get '[JSON] VehicleServiceTierRes)

type PostMerchantConfigVehicleServiceTierUpdate =
  ( "config" :> "vehicleServiceTier" :> "update" :> MandatoryQueryParam "serviceTierType" Dashboard.Common.ServiceTierType
      :> ReqBody
           '[JSON]
           VehicleServiceTierConfigUpdateReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: MerchantUpdateReq -> EulerHS.Types.EulerClient MerchantUpdateRes,
    getMerchantConfigCommon :: EulerHS.Types.EulerClient MerchantCommonConfigRes,
    postMerchantConfigCommonUpdate :: MerchantCommonConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigDriverPool :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> EulerHS.Types.EulerClient DriverPoolConfigRes,
    postMerchantConfigDriverPoolUpdate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> DriverPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigDriverPoolCreate :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Lib.Types.SpecialLocation.Area -> DriverPoolConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigDriverIntelligentPool :: EulerHS.Types.EulerClient DriverIntelligentPoolConfigRes,
    postMerchantConfigDriverIntelligentPoolUpdate :: DriverIntelligentPoolConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantConfigOnboardingDocument :: Kernel.Prelude.Maybe DocumentType -> Kernel.Prelude.Maybe Dashboard.Common.VehicleCategory -> EulerHS.Types.EulerClient DocumentVerificationConfigRes,
    postMerchantConfigOnboardingDocumentUpdate :: DocumentType -> Dashboard.Common.VehicleCategory -> DocumentVerificationConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOnboardingDocumentCreate :: DocumentType -> Dashboard.Common.VehicleCategory -> DocumentVerificationConfigCreateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getMerchantServiceUsageConfig :: EulerHS.Types.EulerClient Dashboard.Common.Merchant.ServiceUsageConfigRes,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceUsageConfigSmsUpdate :: Dashboard.Common.Merchant.SmsServiceUsageConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigVerificationUpdate :: Dashboard.Common.Merchant.VerificationServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance -> Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit -> Kernel.Types.Common.Meters -> CreateFPDriverExtraFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyPerExtraKmRateUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> Kernel.Types.Common.Meters -> UpdateFPPerExtraKmRateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyUpdate :: Kernel.Types.Id.Id Dashboard.Common.FarePolicy -> UpdateFarePolicyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigFarePolicyUpsert :: (Data.ByteString.Lazy.ByteString, UpsertFarePolicyReq) -> EulerHS.Types.EulerClient UpsertFarePolicyResp,
    getMerchantConfigFarePolicyExport :: EulerHS.Types.EulerClient Kernel.Prelude.Text,
    postMerchantConfigOperatingCityCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    postMerchantSchedulerTrigger :: SchedulerTriggerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantUpdateOnboardingVehicleVariantMapping :: (Data.ByteString.Lazy.ByteString, UpdateOnboardingVehicleVariantMappingReq) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigSpecialLocationUpsert ::
      ( Data.ByteString.Lazy.ByteString,
        Dashboard.Common.Merchant.UpsertSpecialLocationCsvReq
      ) ->
      EulerHS.Types.EulerClient Dashboard.Common.Merchant.APISuccessWithUnprocessedEntities,
    getMerchantConfigSpecialLocationList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Types.SpecialLocation.SpecialLocationType -> EulerHS.Types.EulerClient SpecialLocationResp,
    getMerchantConfigGeometryList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient GeometryResp,
    putMerchantConfigGeometryUpdate :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.Merchant.UpdateGeometryReq) -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationUpsert :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation) -> Dashboard.Common.Merchant.UpsertSpecialLocationReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantSpecialLocationGatesUpsert :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Dashboard.Common.Merchant.UpsertSpecialLocationGateReqT -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    deleteMerchantSpecialLocationGatesDelete :: Kernel.Types.Id.Id Lib.Types.SpecialLocation.SpecialLocation -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigClearCacheSubscription :: ClearCacheSubscriptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigUpsertPlanAndConfigSubscription :: UpsertPlanAndConfigReq -> EulerHS.Types.EulerClient UpsertPlanAndConfigResp,
    postMerchantConfigFailover :: Dashboard.Common.Merchant.ConfigNames -> Dashboard.Common.Merchant.ConfigFailoverReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantPayoutConfigUpdate :: PayoutConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantConfigOperatingCityWhiteList :: Dashboard.Common.Merchant.WhiteListOperatingCityReq -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.WhiteListOperatingCityRes,
    postMerchantConfigMerchantCreate :: Dashboard.Common.Merchant.CreateMerchantOperatingCityReqT -> EulerHS.Types.EulerClient Dashboard.Common.Merchant.CreateMerchantOperatingCityRes,
    getMerchantConfigVehicleServiceTier :: Kernel.Prelude.Maybe Dashboard.Common.ServiceTierType -> EulerHS.Types.EulerClient VehicleServiceTierRes,
    postMerchantConfigVehicleServiceTierUpdate :: Dashboard.Common.ServiceTierType -> VehicleServiceTierConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> getMerchantConfigCommon :<|> postMerchantConfigCommonUpdate :<|> getMerchantConfigDriverPool :<|> postMerchantConfigDriverPoolUpdate :<|> postMerchantConfigDriverPoolCreate :<|> getMerchantConfigDriverIntelligentPool :<|> postMerchantConfigDriverIntelligentPoolUpdate :<|> getMerchantConfigOnboardingDocument :<|> postMerchantConfigOnboardingDocumentUpdate :<|> postMerchantConfigOnboardingDocumentCreate :<|> getMerchantServiceUsageConfig :<|> postMerchantServiceConfigMapsUpdate :<|> postMerchantServiceUsageConfigMapsUpdate :<|> postMerchantServiceConfigSmsUpdate :<|> postMerchantServiceUsageConfigSmsUpdate :<|> postMerchantServiceConfigVerificationUpdate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsCreate :<|> postMerchantConfigFarePolicyDriverExtraFeeBoundsUpdate :<|> postMerchantConfigFarePolicyPerExtraKmRateUpdate :<|> postMerchantConfigFarePolicyUpdate :<|> postMerchantConfigFarePolicyUpsert :<|> getMerchantConfigFarePolicyExport :<|> postMerchantConfigOperatingCityCreate :<|> postMerchantSchedulerTrigger :<|> postMerchantUpdateOnboardingVehicleVariantMapping :<|> postMerchantConfigSpecialLocationUpsert :<|> getMerchantConfigSpecialLocationList :<|> getMerchantConfigGeometryList :<|> putMerchantConfigGeometryUpdate :<|> postMerchantSpecialLocationUpsert :<|> deleteMerchantSpecialLocationDelete :<|> postMerchantSpecialLocationGatesUpsert :<|> deleteMerchantSpecialLocationGatesDelete :<|> postMerchantConfigClearCacheSubscription :<|> postMerchantConfigUpsertPlanAndConfigSubscription :<|> postMerchantConfigFailover :<|> postMerchantPayoutConfigUpdate :<|> postMerchantConfigOperatingCityWhiteList :<|> postMerchantConfigMerchantCreate :<|> getMerchantConfigVehicleServiceTier :<|> postMerchantConfigVehicleServiceTierUpdate = merchantClient

data MerchantUserActionType
  = POST_MERCHANT_UPDATE
  | GET_MERCHANT_CONFIG_COMMON
  | POST_MERCHANT_CONFIG_COMMON_UPDATE
  | GET_MERCHANT_CONFIG_DRIVER_POOL
  | POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE
  | POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE
  | GET_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL
  | POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE
  | GET_MERCHANT_CONFIG_ONBOARDING_DOCUMENT
  | POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE
  | POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE
  | GET_MERCHANT_SERVICE_USAGE_CONFIG
  | POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE
  | POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE
  | POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE
  | POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE
  | POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE
  | POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE
  | POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE
  | POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT
  | GET_MERCHANT_CONFIG_FARE_POLICY_EXPORT
  | POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE
  | POST_MERCHANT_SCHEDULER_TRIGGER
  | POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING
  | POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT
  | GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST
  | GET_MERCHANT_CONFIG_GEOMETRY_LIST
  | PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE
  | POST_MERCHANT_SPECIAL_LOCATION_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_DELETE
  | POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT
  | DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE
  | POST_MERCHANT_CONFIG_CLEAR_CACHE_SUBSCRIPTION
  | POST_MERCHANT_CONFIG_UPSERT_PLAN_AND_CONFIG_SUBSCRIPTION
  | POST_MERCHANT_CONFIG_FAILOVER
  | POST_MERCHANT_PAYOUT_CONFIG_UPDATE
  | POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST
  | POST_MERCHANT_CONFIG_MERCHANT_CREATE
  | GET_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER
  | POST_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DocumentType)

$(Data.Singletons.TH.genSingletons [''MerchantUserActionType])
