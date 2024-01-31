{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Merchant
  ( module Dashboard.ProviderPlatform.Merchant,
    module Reexport,
  )
where

import qualified Dashboard.Common as Common
import Dashboard.Common.Merchant as Reexport
import Data.Aeson
import Data.OpenApi hiding (description, name, password, url)
import Data.Text as T
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Predicate
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Types.Value
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant

---------------------------------------------------------
-- merchant update --------------------------------------

type MerchantUpdateAPI =
  "update"
    :> ReqBody '[JSON] MerchantUpdateReq
    :> Post '[JSON] MerchantUpdateRes

data MerchantUpdateReq = MerchantUpdateReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateReq
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MerchantUpdateTReq = MerchantUpdateTReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool,
    exoPhones :: Maybe (NonEmpty ExophoneReq),
    fcmConfig :: Maybe FCMConfigUpdateTReq
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

validateMerchantUpdateReq :: Validate MerchantUpdateReq
validateMerchantUpdateReq MerchantUpdateReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name,
      whenJust exoPhones $ \phones -> do
        sequenceA_
          [ validateField "exoPhones" phones $ UniqueField @"primaryPhone",
            validateField "exoPhones" phones $ UniqueField @"backupPhone"
          ],
      whenJust exoPhones $ \phones -> for_ phones $ \exophoneReq -> do
        validateObject "exoPhones" exophoneReq validateExophoneReq,
      whenJust fcmConfig $ \cfg -> validateObject "fcmConfig" cfg validateFCMConfigUpdateReq
    ]

data MerchantUpdateRes = MerchantUpdateRes
  { name :: Text,
    description :: Maybe Text,
    contactNumber :: Maybe Text,
    status :: Status,
    enabled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }

---------------------------------------------------------
-- merchant common config -------------------------------

type MerchantCommonConfigAPI =
  "config"
    :> "common"
    :> Get '[JSON] MerchantCommonConfigRes

data MerchantCommonConfigRes = MerchantCommonConfigRes
  { pickupLocThreshold :: Meters,
    dropLocThreshold :: Meters,
    rideTimeEstimatedThreshold :: Seconds,
    includeDriverCurrentlyOnRide :: Bool,
    defaultPopupDelay :: Seconds,
    popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int,
    mediaFileUrlPattern :: Text,
    mediaFileSizeUpperLimit :: Int,
    onboardingTryLimit :: Int,
    onboardingRetryTimeInHours :: Int,
    checkImageExtractionForDashboard :: Bool,
    searchRepeatLimit :: Int,
    actualRideDistanceDiffThreshold :: HighPrecMeters,
    upwardsRecomputeBuffer :: HighPrecMeters,
    approxRideDistanceDiffThreshold :: HighPrecMeters,
    minLocationAccuracy :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant common config update ------------------------

type MerchantCommonConfigUpdateAPI =
  "config"
    :> "common"
    :> "update"
    :> ReqBody '[JSON] MerchantCommonConfigUpdateReq
    :> Post '[JSON] APISuccess

data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
  { pickupLocThreshold :: Maybe (MandatoryValue Meters),
    dropLocThreshold :: Maybe (MandatoryValue Meters),
    rideTimeEstimatedThreshold :: Maybe (MandatoryValue Seconds),
    defaultPopupDelay :: Maybe (MandatoryValue Seconds),
    popupDelayToAddAsPenalty :: Maybe (OptionalValue Seconds),
    thresholdCancellationScore :: Maybe (OptionalValue Int),
    minRidesForCancellationScore :: Maybe (OptionalValue Int),
    mediaFileUrlPattern :: Maybe (MandatoryValue Text),
    mediaFileSizeUpperLimit :: Maybe (MandatoryValue Int),
    onboardingTryLimit :: Maybe (MandatoryValue Int),
    onboardingRetryTimeInHours :: Maybe (MandatoryValue Int),
    checkImageExtractionForDashboard :: Maybe (MandatoryValue Bool),
    searchRepeatLimit :: Maybe (MandatoryValue Int),
    driverPaymentCycleBuffer :: Maybe (MandatoryValue NominalDiffTime), -- TODO : Add in validation if reqd
    driverPaymentCycleDuration :: Maybe (MandatoryValue NominalDiffTime),
    driverPaymentCycleStartTime :: Maybe (MandatoryValue NominalDiffTime),
    driverPaymentReminderInterval :: Maybe (MandatoryValue NominalDiffTime),
    timeDiffFromUtc :: Maybe (MandatoryValue Seconds),
    driverAutoPayNotificationTime :: Maybe (MandatoryValue NominalDiffTime),
    driverAutoPayExecutionTime :: Maybe (MandatoryValue NominalDiffTime),
    driverFeeMandateNotificationBatchSize :: Maybe (MandatoryValue Int),
    driverFeeMandateExecutionBatchSize :: Maybe (MandatoryValue Int),
    mandateNotificationRescheduleInterval :: Maybe (MandatoryValue NominalDiffTime),
    mandateExecutionRescheduleInterval :: Maybe (MandatoryValue NominalDiffTime),
    driverFeeCalculationTime :: Maybe (OptionalValue NominalDiffTime),
    driverFeeCalculatorBatchSize :: Maybe (OptionalValue Int),
    driverFeeCalculatorBatchGap :: Maybe (OptionalValue NominalDiffTime),
    orderAndNotificationStatusCheckTime :: Maybe (OptionalValue NominalDiffTime),
    orderAndNotificationStatusCheckTimeLimit :: Maybe (OptionalValue NominalDiffTime),
    snapToRoadConfidenceThreshold :: Maybe (MandatoryValue Double),
    useWithSnapToRoadFallback :: Maybe (MandatoryValue Bool)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantCommonConfigUpdateReq where
  hideSecrets = identity

validateMerchantCommonConfigUpdateReq :: Validate MerchantCommonConfigUpdateReq
validateMerchantCommonConfigUpdateReq MerchantCommonConfigUpdateReq {..} =
  sequenceA_
    [ validateField "pickupLocThreshold" pickupLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "dropLocThreshold" dropLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "defaultPopupDelay" defaultPopupDelay $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "popupDelayToAddAsPenalty" popupDelayToAddAsPenalty $ InMaybe $ InValue $ Min @Seconds 0,
      validateField "thresholdCancellationScore" thresholdCancellationScore $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "mediaFileUrlPattern" mediaFileUrlPattern $ InMaybe $ InValue $ MinLength 1,
      validateField "mediaFileSizeUpperLimit" mediaFileSizeUpperLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "minRidesForCancellationScore" minRidesForCancellationScore $ InMaybe $ InValue $ Min @Int 0,
      validateField "onboardingTryLimit" onboardingTryLimit $ InMaybe $ InValue $ Min @Int 0,
      validateField "onboardingRetryTimeInHours" onboardingRetryTimeInHours $ InMaybe $ InValue $ Min @Int 0,
      validateField "searchRepeatLimit" searchRepeatLimit $ InMaybe $ InValue $ Min @Int 0
      -- validateField "driverPaymentCycleDuration" driverPaymentCycleDuration $ InMaybe $ InValue $ Min @NominalDiffTime 86400,
      -- validateField "driverPaymentCycleStartTime" driverPaymentCycleStartTime $ InMaybe $ InValue $ InRange @NominalDiffTime 0 86399,
      -- validateField "timeDiffFromUtc" timeDiffFromUtc $ InMaybe $ InValue $ InRange @Seconds maxWestWardTimeDiff maxEastWardTimeDiff, --Set -12 hrs to 14 hrs as the max time diff from UTC is -12 Hrs to 14 Hrs
      -- validateField "driverPaymentCycleBuffer" driverPaymentCycleBuffer $ InMaybe $ InValue $ Max @NominalDiffTime (cycleBufferTimeLimit driverPaymentCycleDuration),
      -- validateField "driverPaymentReminderInterval" driverPaymentReminderInterval $ InMaybe $ InValue $ Max @NominalDiffTime (reminderIntervalLimit driverPaymentCycleBuffer)
    ]

-- where
--   maxWestWardTimeDiff = -12 * 3600
--   maxEastWardTimeDiff = 14 * 3600
--   cycleBufferTimeLimit mbMValue =
--     let mValue = fromMaybe (MandatoryValue 86400) mbMValue
--      in (mValue.value - 8 * 3600)
--   reminderIntervalLimit mbMValue =
--     let mValue = fromMaybe (MandatoryValue 14400) mbMValue
--      in (mValue.value * 0.5)

---------------------------------------------------------
-- merchant driver pool config  -------------------------

type DriverPoolConfigAPI =
  "config"
    :> "driverPool"
    :> QueryParam "tripDistance" Meters
    :> Get '[JSON] DriverPoolConfigRes

type DriverPoolConfigRes = [DriverPoolConfigItem]

data DriverPoolConfigItem = DriverPoolConfigItem
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    driverRequestCountLimit :: Int,
    driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    maxParallelSearchRequests :: Int,
    poolSortingType :: PoolSortingType,
    singleBatchProcessTime :: Seconds,
    tripDistance :: Meters,
    radiusShrinkValueForDriversOnRide :: Meters,
    driverToDestinationDistanceThreshold :: Meters,
    driverToDestinationDuration :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PoolSortingType = Intelligent | Random
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver pool config update -------------------

type DriverPoolConfigUpdateAPI =
  "config"
    :> "driverPool"
    :> "update"
    :> MandatoryQueryParam "tripDistance" Meters
    :> QueryParam "vehicleVariant" Variant
    :> QueryParam "tripCategory" Text
    :> ReqBody '[JSON] DriverPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
  { minRadiusOfSearch :: Maybe (MandatoryValue Meters),
    maxRadiusOfSearch :: Maybe (MandatoryValue Meters),
    radiusStepSize :: Maybe (MandatoryValue Meters),
    driverPositionInfoExpiry :: Maybe (OptionalValue Seconds),
    actualDistanceThreshold :: Maybe (OptionalValue Meters),
    maxDriverQuotesRequired :: Maybe (MandatoryValue Int),
    driverQuoteLimit :: Maybe (MandatoryValue Int),
    driverRequestCountLimit :: Maybe (MandatoryValue Int),
    driverBatchSize :: Maybe (MandatoryValue Int),
    maxNumberOfBatches :: Maybe (MandatoryValue Int),
    maxParallelSearchRequests :: Maybe (MandatoryValue Int),
    poolSortingType :: Maybe (MandatoryValue PoolSortingType),
    singleBatchProcessTime :: Maybe (MandatoryValue Seconds),
    distanceBasedBatchSplit :: Maybe (MandatoryValue [BatchSplitByPickupDistance])
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverPoolConfigUpdateReq :: Validate DriverPoolConfigUpdateReq
validateDriverPoolConfigUpdateReq DriverPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ InMaybe $ InValue $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ InMaybe $ InValue $ Min @Meters (maybe 1 (.value) minRadiusOfSearch),
      validateField "radiusStepSize" radiusStepSize $ InMaybe $ InValue $ Min @Meters 1,
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ InValue $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ InMaybe $ InValue $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ InMaybe $ InValue $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver pool config create -------------------

type DriverPoolConfigCreateAPI =
  "config"
    :> "driverPool"
    :> "create"
    :> MandatoryQueryParam "tripDistance" Meters
    :> QueryParam "vehiclevariant" Variant
    :> QueryParam "tripCategory" Text
    :> ReqBody '[JSON] DriverPoolConfigCreateReq
    :> Post '[JSON] APISuccess

data BatchSplitByPickupDistance = BatchSplitByPickupDistance
  { batchSplitSize :: Int,
    batchSplitDelay :: Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverPoolConfigCreateReq = DriverPoolConfigCreateReq
  { minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    driverRequestCountLimit :: Int,
    driverBatchSize :: Int,
    maxNumberOfBatches :: Int,
    maxParallelSearchRequests :: Int,
    poolSortingType :: PoolSortingType,
    distanceBasedBatchSplit :: [BatchSplitByPickupDistance],
    singleBatchProcessTime :: Seconds,
    radiusShrinkValueForDriversOnRide :: Meters,
    driverToDestinationDistanceThreshold :: Meters,
    driverToDestinationDuration :: Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigCreateReq where
  hideSecrets = identity

validateDriverPoolConfigCreateReq :: Validate DriverPoolConfigCreateReq
validateDriverPoolConfigCreateReq DriverPoolConfigCreateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ Min @Meters minRadiusOfSearch,
      validateField "radiusStepSize" radiusStepSize $ Min @Meters 1,
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ Min @Meters 0,
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ Min @Seconds 1,
      validateField "radiusShrinkValueForDriversOnRide" radiusShrinkValueForDriversOnRide $ Min @Meters 1,
      validateField "driverToDestinationDistanceThreshold" driverToDestinationDistanceThreshold $ Min @Meters 1,
      validateField "driverToDestinationDuration" driverToDestinationDuration $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver intelligent pool config --------------

type DriverIntelligentPoolConfigAPI =
  "config"
    :> "driverIntelligentPool"
    :> Get '[JSON] DriverIntelligentPoolConfigRes

data DriverIntelligentPoolConfigRes = DriverIntelligentPoolConfigRes
  { availabilityTimeWeightage :: Int,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Int,
    acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Int,
    cancellationAndRideFrequencyRatioWindowOption :: SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe Int,
    speedNormalizer :: Double,
    driverSpeedWeightage :: Int,
    minLocationUpdates :: Int,
    locationUpdateSampleTime :: Minutes,
    defaultDriverSpeed :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- merchant driver intelligent pool config update -------

type DriverIntelligentPoolConfigUpdateAPI =
  "config"
    :> "driverIntelligentPool"
    :> "update"
    :> ReqBody '[JSON] DriverIntelligentPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
  { availabilityTimeWeightage :: Maybe (MandatoryValue Int),
    availabilityTimeWindowOption :: Maybe SWC.SlidingWindowOptions, -- value wrapper make no sense for lists and objects
    acceptanceRatioWeightage :: Maybe (MandatoryValue Int),
    acceptanceRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Maybe (MandatoryValue Int),
    cancellationAndRideFrequencyRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Maybe (MandatoryValue Int),
    minQuotesToQualifyForIntelligentPoolWindowOption :: Maybe SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe (OptionalValue Int),
    speedNormalizer :: Maybe (MandatoryValue Double),
    driverSpeedWeightage :: Maybe (MandatoryValue Int),
    minLocationUpdates :: Maybe (MandatoryValue Int),
    locationUpdateSampleTime :: Maybe (MandatoryValue Minutes),
    defaultDriverSpeed :: Maybe (MandatoryValue Double)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverIntelligentPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverIntelligentPoolConfigUpdateReq :: Validate DriverIntelligentPoolConfigUpdateReq
validateDriverIntelligentPoolConfigUpdateReq DriverIntelligentPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "availabilityTimeWeightage" availabilityTimeWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust availabilityTimeWindowOption $ \obj ->
        validateObject "availabilityTimeWindowOption" obj validateSlidingWindowOptions,
      validateField "acceptanceRatioWeightage" acceptanceRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust acceptanceRatioWindowOption $ \obj ->
        validateObject "acceptanceRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "cancellationRatioWeightage" cancellationRatioWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      whenJust cancellationAndRideFrequencyRatioWindowOption $ \obj ->
        validateObject "cancellationAndRideFrequencyRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "minQuotesToQualifyForIntelligentPool" minQuotesToQualifyForIntelligentPool $ InMaybe $ InValue $ Min @Int 1,
      whenJust minQuotesToQualifyForIntelligentPoolWindowOption $ \obj ->
        validateObject "minQuotesToQualifyForIntelligentPoolWindowOption" obj validateSlidingWindowOptions,
      validateField "intelligentPoolPercentage" intelligentPoolPercentage $ InMaybe $ InValue $ InRange @Int 0 100,
      validateField "speedNormalizer" speedNormalizer $ InMaybe $ InValue $ Min @Double 0.0,
      validateField "driverSpeedWeightage" driverSpeedWeightage $ InMaybe $ InValue $ InRange @Int (-100) 100,
      validateField "minLocationUpdates" minLocationUpdates $ InMaybe $ InValue $ Min @Int 0,
      validateField "locationUpdateSampleTime" locationUpdateSampleTime $ InMaybe $ InValue $ Min @Minutes 0,
      validateField "defaultDriverSpeed" defaultDriverSpeed $ InMaybe $ InValue $ Min @Double 0.0
    ]

validateSlidingWindowOptions :: Validate SWC.SlidingWindowOptions
validateSlidingWindowOptions SWC.SlidingWindowOptions {..} =
  validateField "period" period $ Min @Integer 0

---------------------------------------------------------
-- merchant onboarding document config update -----------

type OnboardingDocumentConfigAPI =
  "config"
    :> "onboardingDocument"
    :> QueryParam "documentType" DocumentType
    :> Get '[JSON] OnboardingDocumentConfigRes

type OnboardingDocumentConfigRes = [OnboardingDocumentConfigItem]

data OnboardingDocumentConfigItem = OnboardingDocumentConfigItem
  { documentType :: DocumentType,
    checkExtraction :: Bool,
    checkExpiry :: Bool,
    supportedVehicleClasses :: SupportedVehicleClasses,
    vehicleClassCheckType :: VehicleClassCheckType,
    rcNumberPrefix :: Text,
    rcNumberPrefixList :: Maybe [Text],
    maxRetryCount :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SupportedVehicleClasses = DLValidClasses [Text] | RCValidClasses [VehicleClassVariantMap]
  deriving stock (Generic, Show)

instance ToJSON SupportedVehicleClasses where
  toJSON = genericToJSON supportedVCOptions

instance FromJSON SupportedVehicleClasses where
  parseJSON = genericParseJSON supportedVCOptions

instance ToSchema SupportedVehicleClasses where
  declareNamedSchema = genericDeclareNamedSchema supportedVCSchemaOptions

supportedVCOptions :: Options
supportedVCOptions =
  defaultOptions
    { sumEncoding = taggedObject,
      constructorTagModifier = modifier
    }

supportedVCSchemaOptions :: SchemaOptions
supportedVCSchemaOptions =
  defaultSchemaOptions
    { sumEncoding = taggedObject,
      constructorTagModifier = modifier
    }

taggedObject :: SumEncoding
taggedObject =
  TaggedObject
    { tagFieldName = "documentType",
      contentsFieldName = "vehicleClasses"
    }

modifier :: String -> String
modifier = \case
  "DL" -> "DLValidClasses"
  "RC" -> "RCValidClasses"
  x -> x

data VehicleClassVariantMap = VehicleClassVariantMap
  { vehicleClass :: Text,
    vehicleCapacity :: Maybe Int,
    vehicleVariant :: Variant,
    manufacturer :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleClassCheckType = Infix | Prefix | Suffix
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentType = RC | DL | RCInsurance
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''DocumentType)

---------------------------------------------------------
-- merchant onboarding document config update -----------

type OnboardingDocumentConfigUpdateAPI =
  "config"
    :> "onboardingDocument"
    :> "update"
    :> MandatoryQueryParam "documentType" DocumentType
    :> ReqBody '[JSON] OnboardingDocumentConfigUpdateReq
    :> Post '[JSON] APISuccess

data OnboardingDocumentConfigUpdateReq = OnboardingDocumentConfigUpdateReq
  { checkExtraction :: Maybe (MandatoryValue Bool),
    checkExpiry :: Maybe (MandatoryValue Bool),
    supportedVehicleClasses :: Maybe SupportedVehicleClasses, -- value wrapper make no sense for lists and objects
    rcNumberPrefix :: Maybe (MandatoryValue Text),
    rcNumberPrefixList :: Maybe (MandatoryValue [Text]),
    maxRetryCount :: Maybe (MandatoryValue Int),
    vehicleClassCheckType :: Maybe (MandatoryValue VehicleClassCheckType)
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigUpdateReq where
  hideSecrets = identity

---------------------------------------------------------
-- merchant onboarding document config create -----------

type OnboardingDocumentConfigCreateAPI =
  "config"
    :> "onboardingDocument"
    :> "create"
    :> MandatoryQueryParam "documentType" DocumentType
    :> ReqBody '[JSON] OnboardingDocumentConfigCreateReq
    :> Post '[JSON] APISuccess

data OnboardingDocumentConfigCreateReq = OnboardingDocumentConfigCreateReq
  { checkExtraction :: Bool,
    checkExpiry :: Bool,
    supportedVehicleClasses :: SupportedVehicleClasses,
    rcNumberPrefix :: Text,
    rcNumberPrefixList :: Maybe [Text],
    maxRetryCount :: Int,
    vehicleClassCheckType :: VehicleClassCheckType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigCreateReq where
  hideSecrets = identity

---------------------------------------------------------
-- Create Fare Policy -----------------------------------

type CreateFPDriverExtraFee =
  "config"
    :> "farePolicy"
    :> Capture "farePolicyId" (Id Common.FarePolicy)
    :> "driverExtraFeeBounds"
    :> "create"
    :> MandatoryQueryParam "startDistance" Meters
    :> ReqBody '[JSON] CreateFPDriverExtraFeeReq
    :> Post '[JSON] APISuccess

data CreateFPDriverExtraFeeReq = CreateFPDriverExtraFeeReq
  { minFee :: Money,
    maxFee :: Money
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CreateFPDriverExtraFeeReq where
  hideSecrets = identity

---------------------------------------------------------
-- Update Fare Policy -----------------------------------

type UpdateFPDriverExtraFee =
  "config"
    :> "farePolicy"
    :> Capture "farePolicyId" (Id Common.FarePolicy)
    :> "driverExtraFeeBounds"
    :> "update"
    :> MandatoryQueryParam "startDistance" Meters
    :> ReqBody '[JSON] CreateFPDriverExtraFeeReq
    :> Post '[JSON] APISuccess

type UpdateFPPerExtraKmRate =
  "config"
    :> "farePolicy"
    :> Capture "farePolicyId" (Id Common.FarePolicy)
    :> Capture "startDistance" Meters
    :> "perExtraKmRate"
    :> "update"
    :> ReqBody '[JSON] UpdateFPPerExtraKmRateReq
    :> Post '[JSON] APISuccess

newtype UpdateFPPerExtraKmRateReq = UpdateFPPerExtraKmRateReq
  { perExtraKmRate :: HighPrecMoney
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateFPPerExtraKmRateReq where
  hideSecrets = identity

type UpdateFarePolicy =
  "config"
    :> "farePolicy"
    :> Capture "farePolicyId" (Id Common.FarePolicy)
    :> "update"
    :> ReqBody '[JSON] UpdateFarePolicyReq
    :> Post '[JSON] APISuccess

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { serviceCharge :: Maybe Money,
    nightShiftBounds :: Maybe NightShiftBounds,
    allowedTripDistanceBounds :: Maybe AllowedTripDistanceBounds,
    govtCharges :: Maybe Double,
    perMinuteRideExtraTimeCharge :: Maybe HighPrecMoney,
    description :: Maybe Text,
    baseDistance :: Maybe Meters,
    baseFare :: Maybe Money,
    deadKmFare :: Maybe Money,
    waitingCharge :: Maybe WaitingCharge,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    freeWaitingTime :: Maybe Minutes,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateFarePolicyReq where
  hideSecrets = identity

data WaitingChargeInfo = WaitingChargeInfo
  { freeWaitingTime :: Minutes,
    waitingCharge :: WaitingCharge
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema, Read)

data WaitingCharge = PerMinuteWaitingCharge HighPrecMoney | ConstantWaitingCharge Money
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NightShiftCharge = ProgressiveNightShiftCharge Float | ConstantNightShiftCharge Money
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data NightShiftBounds = NightShiftBounds
  { nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

data AllowedTripDistanceBounds = AllowedTripDistanceBounds
  { maxAllowedTripDistance :: Meters,
    minAllowedTripDistance :: Meters
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForJSON ''NightShiftCharge)
$(mkBeamInstancesForJSON ''WaitingCharge)

---- generic trigger for schedulers ----

type SchedulerTriggerAPI =
  "scheduler"
    :> "trigger"
    :> ReqBody '[JSON] SchedulerTriggerReq
    :> Post '[JSON] APISuccess

data SchedulerTriggerReq = SchedulerTriggerReq
  { scheduledAt :: Maybe UTCTime,
    jobName :: Maybe JobName,
    jobData :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JobName = BadDebtCalculationTrigger | DriverFeeCalculationTrigger
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets SchedulerTriggerReq where
  hideSecrets = identity
