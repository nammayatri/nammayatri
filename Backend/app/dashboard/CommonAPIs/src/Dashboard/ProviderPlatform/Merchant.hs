{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Dashboard.ProviderPlatform.Merchant
  ( module Dashboard.ProviderPlatform.Merchant,
    module Reexport,
  )
where

import Dashboard.Common.Merchant as Reexport
import Data.Aeson
import qualified Data.Bifunctor as BF
import Data.ByteString.Lazy as BSL
import Data.Text as T
import Data.Text.Encoding as DT
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Predicate
import qualified Kernel.Types.SlidingWindowCounters as SWC
import qualified Kernel.Utils.Predicates as P
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
-- merchant common config update ------------------------

type MerchantCommonConfigUpdateAPI =
  "config"
    :> "common"
    :> "update"
    :> ReqBody '[JSON] MerchantCommonConfigUpdateReq
    :> Post '[JSON] APISuccess

data MerchantCommonConfigUpdateReq = MerchantCommonConfigUpdateReq
  { pickupLocThreshold :: Maybe Meters,
    dropLocThreshold :: Maybe Meters,
    rideTimeEstimatedThreshold :: Maybe Seconds,
    defaultPopupDelay :: Maybe Seconds,
    popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int,
    waitingTimeEstimatedThreshold :: Maybe Seconds,
    onboardingTryLimit :: Maybe Int,
    onboardingRetryTimeInHours :: Maybe Int,
    checkImageExtractionForDashboard :: Maybe Bool,
    searchRepeatLimit :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MerchantCommonConfigUpdateReq where
  hideSecrets = identity

validateMerchantCommonConfigUpdateReq :: Validate MerchantCommonConfigUpdateReq
validateMerchantCommonConfigUpdateReq MerchantCommonConfigUpdateReq {..} =
  sequenceA_
    [ validateField "pickupLocThreshold" pickupLocThreshold $ InMaybe $ Min @Meters 0,
      validateField "dropLocThreshold" dropLocThreshold $ InMaybe $ Min @Meters 0,
      validateField "defaultPopupDelay" defaultPopupDelay $ InMaybe $ Min @Seconds 0,
      validateField "popupDelayToAddAsPenalty" popupDelayToAddAsPenalty $ InMaybe $ Min @Seconds 0,
      validateField "thresholdCancellationScore" thresholdCancellationScore $ InMaybe $ InRange @Int 0 100,
      validateField "minRidesForCancellationScore" minRidesForCancellationScore $ InMaybe $ Min @Int 0,
      validateField "waitingTimeEstimatedThreshold" waitingTimeEstimatedThreshold $ InMaybe $ Min @Seconds 0,
      validateField "onboardingTryLimit" onboardingTryLimit $ InMaybe $ Min @Int 0,
      validateField "onboardingRetryTimeInHours" onboardingRetryTimeInHours $ InMaybe $ Min @Int 0,
      validateField "searchRepeatLimit" searchRepeatLimit $ InMaybe $ Min @Int 1
    ]

---------------------------------------------------------
-- merchant driver pool config update -------------------

type DriverPoolConfigUpdateAPI =
  "config"
    :> "driverPool"
    :> "update"
    :> MandatoryQueryParam "tripDistance" Meters
    :> ReqBody '[JSON] DriverPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverPoolConfigUpdateReq = DriverPoolConfigUpdateReq
  { minRadiusOfSearch :: Maybe Meters,
    maxRadiusOfSearch :: Maybe Meters,
    radiusStepSize :: Maybe Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Maybe Int,
    driverQuoteLimit :: Maybe Int,
    driverRequestCountLimit :: Maybe Int,
    driverBatchSize :: Maybe Int,
    maxNumberOfBatches :: Maybe Int,
    maxParallelSearchRequests :: Maybe Int,
    poolSortingType :: Maybe PoolSortingType,
    singleBatchProcessTime :: Maybe Seconds
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverPoolConfigUpdateReq where
  hideSecrets = identity

data PoolSortingType = Intelligent | Random
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateDriverPoolConfigUpdateReq :: Validate DriverPoolConfigUpdateReq
validateDriverPoolConfigUpdateReq DriverPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ InMaybe $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ InMaybe $ Min @Meters (fromMaybe 1 minRadiusOfSearch),
      validateField "radiusStepSize" radiusStepSize $ InMaybe $ Min @Meters 1,
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ Min @Meters 0,
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ InMaybe $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ InMaybe $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ InMaybe $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ InMaybe $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ InMaybe $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ InMaybe $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ InMaybe $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver pool config create -------------------

type DriverPoolConfigCreateAPI =
  "config"
    :> "driverPool"
    :> "create"
    :> MandatoryQueryParam "tripDistance" Meters
    :> ReqBody '[JSON] DriverPoolConfigCreateReq
    :> Post '[JSON] APISuccess

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
    singleBatchProcessTime :: Seconds
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
      validateField "singleBatchProcessTime" singleBatchProcessTime $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver intelligent pool config update -------

type DriverIntelligentPoolConfigUpdateAPI =
  "config"
    :> "driverIntelligentPool"
    :> "update"
    :> ReqBody '[JSON] DriverIntelligentPoolConfigUpdateReq
    :> Post '[JSON] APISuccess

data DriverIntelligentPoolConfigUpdateReq = DriverIntelligentPoolConfigUpdateReq
  { availabilityTimeWeightage :: Maybe Int,
    availabilityTimeWindowOption :: Maybe SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Maybe Int,
    acceptanceRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Maybe Int,
    cancellationRatioWindowOption :: Maybe SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Maybe Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: Maybe SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DriverIntelligentPoolConfigUpdateReq where
  hideSecrets = identity

validateDriverIntelligentPoolConfigUpdateReq :: Validate DriverIntelligentPoolConfigUpdateReq
validateDriverIntelligentPoolConfigUpdateReq DriverIntelligentPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "availabilityTimeWeightage" availabilityTimeWeightage $ InMaybe $ Min @Int 0,
      whenJust availabilityTimeWindowOption $ \obj ->
        validateObject "availabilityTimeWindowOption" obj validateSlidingWindowOptions,
      validateField "acceptanceRatioWeightage" acceptanceRatioWeightage $ InMaybe $ Min @Int 0,
      whenJust acceptanceRatioWindowOption $ \obj ->
        validateObject "acceptanceRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "cancellationRatioWeightage" cancellationRatioWeightage $ InMaybe $ Min @Int 0,
      whenJust cancellationRatioWindowOption $ \obj ->
        validateObject "cancellationRatioWindowOption" obj validateSlidingWindowOptions,
      validateField "minQuotesToQualifyForIntelligentPool" minQuotesToQualifyForIntelligentPool $ InMaybe $ Min @Int 1,
      whenJust minQuotesToQualifyForIntelligentPoolWindowOption $ \obj ->
        validateObject "minQuotesToQualifyForIntelligentPoolWindowOption" obj validateSlidingWindowOptions,
      validateField "intelligentPoolPercentage" intelligentPoolPercentage $ InMaybe $ InRange @Int 0 100
    ]

validateSlidingWindowOptions :: Validate SWC.SlidingWindowOptions
validateSlidingWindowOptions SWC.SlidingWindowOptions {..} =
  validateField "period" period $ Min @Integer 0

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
  { checkExtraction :: Maybe Bool,
    checkExpiry :: Maybe Bool,
    validVehicleClasses :: Maybe [Text],
    vehicleClassCheckType :: Maybe VehicleClassCheckType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigUpdateReq where
  hideSecrets = identity

data VehicleClassCheckType = Infix | Prefix | Suffix
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DocumentType = RC | DL | RCInsurance
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToParamSchema)

instance FromHttpApiData DocumentType where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData DocumentType where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode

validateOnboardingDocumentConfigUpdateReq :: Validate OnboardingDocumentConfigUpdateReq
validateOnboardingDocumentConfigUpdateReq OnboardingDocumentConfigUpdateReq {..} =
  validateField "validVehicleClasses" validVehicleClasses $ InMaybe $ InList $ MinLength 1

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
    validVehicleClasses :: [Text],
    vehicleClassCheckType :: VehicleClassCheckType
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets OnboardingDocumentConfigCreateReq where
  hideSecrets = identity

validateOnboardingDocumentConfigCreateReq :: Validate OnboardingDocumentConfigCreateReq
validateOnboardingDocumentConfigCreateReq OnboardingDocumentConfigCreateReq {..} =
  validateField "validVehicleClasses" validVehicleClasses $ InList $ MinLength 1
