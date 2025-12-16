{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.Merchant
  ( module Dashboard.ProviderPlatform.Management.Merchant,
    module ReExport,
  )
where

import API.Types.ProviderPlatform.Management.Endpoints.Merchant as Reexport
import Dashboard.Common as ReExport
import Dashboard.Common.Merchant
import Data.Aeson
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.Common
import Kernel.Types.Predicate
import qualified Kernel.Types.SlidingWindowCounters as SWC
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation

---------------------------------------------------------
-- merchant update --------------------------------------

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

instance HideSecrets MerchantUpdateReq where
  type ReqWithoutSecrets MerchantUpdateReq = MerchantUpdateTReq
  hideSecrets MerchantUpdateReq {..} =
    MerchantUpdateTReq
      { fcmConfig = hideSecrets <$> fcmConfig,
        ..
      }

---------------------------------------------------------
-- merchant common config -------------------------------

validateMerchantCommonConfigUpdateReq :: Validate MerchantCommonConfigUpdateReq
validateMerchantCommonConfigUpdateReq MerchantCommonConfigUpdateReq {..} =
  sequenceA_
    [ validateField "pickupLocThreshold" pickupLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "dropLocThreshold" dropLocThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "pickupLocThresholdWithUnit" pickupLocThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "dropLocThresholdWithUnit" dropLocThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
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
-- merchant driver pool config update -------------------

validateDriverPoolConfigUpdateReq :: Validate DriverPoolConfigUpdateReq
validateDriverPoolConfigUpdateReq DriverPoolConfigUpdateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ InMaybe $ InValue $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ InMaybe $ InValue $ Min @Meters (maybe 1 (.value) minRadiusOfSearch),
      validateField "radiusStepSize" radiusStepSize $ InMaybe $ InValue $ Min @Meters 1,
      validateField "minRadiusOfSearchWithUnit" minRadiusOfSearchWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 1 Meter),
      validateField "maxRadiusOfSearchWithUnit" maxRadiusOfSearchWithUnit $ InMaybe $ InValue $ Min @Distance (maybe (Distance 1 Meter) (.value) minRadiusOfSearchWithUnit),
      validateField "radiusStepSizeWithUnit" radiusStepSizeWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 1 Meter),
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ InValue $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ InValue $ Min @Meters 0,
      validateField "actualDistanceThresholdOnRide" actualDistanceThresholdOnRide $ InMaybe $ InValue $ Min @Meters 0,
      validateField "actualDistanceThresholdWithUnit" actualDistanceThresholdWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "actualDistanceThresholdOnRideWithUnit" actualDistanceThresholdOnRideWithUnit $ InMaybe $ InValue $ Min @Distance (Distance 0 Meter),
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ InMaybe $ InValue $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ InMaybe $ InValue $ Min @Int 1,
      validateField "maxParallelSearchRequestsOnRide" maxParallelSearchRequestsOnRide $ InMaybe $ InValue $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ InMaybe $ InValue $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver pool config create -------------------

validateDriverPoolConfigCreateReq :: Validate DriverPoolConfigCreateReq
validateDriverPoolConfigCreateReq DriverPoolConfigCreateReq {..} =
  sequenceA_
    [ validateField "minRadiusOfSearch" minRadiusOfSearch $ Min @Meters 1,
      validateField "maxRadiusOfSearch" maxRadiusOfSearch $ Min @Meters minRadiusOfSearch,
      validateField "radiusStepSize" radiusStepSize $ Min @Meters 1,
      validateField "minRadiusOfSearchWithUnit" minRadiusOfSearchWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "maxRadiusOfSearchWithUnit" maxRadiusOfSearchWithUnit $ InMaybe $ Min @Distance $ fromMaybe (convertMetersToDistance Meter minRadiusOfSearch) minRadiusOfSearchWithUnit,
      validateField "radiusStepSizeWithUnit" radiusStepSizeWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverPositionInfoExpiry" driverPositionInfoExpiry $ InMaybe $ Min @Seconds 1,
      validateField "actualDistanceThreshold" actualDistanceThreshold $ InMaybe $ Min @Meters 0,
      validateField "actualDistanceThresholdOnRide" actualDistanceThresholdOnRide $ InMaybe $ Min @Meters 0,
      validateField "actualDistanceThresholdWithUnit" actualDistanceThresholdWithUnit $ InMaybe $ Min @Distance (Distance 0 Meter),
      validateField "actualDistanceThresholdOnRideWithUnit" actualDistanceThresholdOnRideWithUnit $ InMaybe $ Min @Distance (Distance 0 Meter),
      validateField "maxDriverQuotesRequired" maxDriverQuotesRequired $ Min @Int 1,
      validateField "driverQuoteLimit" driverQuoteLimit $ Min @Int 1,
      validateField "driverRequestCountLimit" driverRequestCountLimit $ Min @Int 1,
      validateField "driverBatchSize" driverBatchSize $ Min @Int 1,
      validateField "maxNumberOfBatches" maxNumberOfBatches $ Min @Int 1,
      validateField "maxParallelSearchRequests" maxParallelSearchRequests $ Min @Int 1,
      validateField "maxParallelSearchRequestsOnRide" maxParallelSearchRequestsOnRide $ Min @Int 1,
      validateField "singleBatchProcessTime" singleBatchProcessTime $ Min @Seconds 1,
      validateField "radiusShrinkValueForDriversOnRide" radiusShrinkValueForDriversOnRide $ Min @Meters 1,
      validateField "driverToDestinationDistanceThreshold" driverToDestinationDistanceThreshold $ Min @Meters 1,
      validateField "radiusShrinkValueForDriversOnRideWithUnit" radiusShrinkValueForDriversOnRideWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverToDestinationDistanceThresholdWithUnit" driverToDestinationDistanceThresholdWithUnit $ InMaybe $ Min @Distance (Distance 1 Meter),
      validateField "driverToDestinationDuration" driverToDestinationDuration $ Min @Seconds 1
    ]

---------------------------------------------------------
-- merchant driver intelligent pool config update -------

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
-- fare policy update ------------------------------------

validateUpdateFarePolicyReq :: Validate UpdateFarePolicyReq
validateUpdateFarePolicyReq UpdateFarePolicyReq {..} =
  sequenceA_
    [ validateField "businessDiscountPercentage" businessDiscountPercentage $ InMaybe $ InRange @Double 1.0 100.0
    ]

---------------------------------------------------------
-- merchant onboarding document config update -----------

--- Upsert fare policy using csv file ----

instance FromMultipart Tmp UpsertFarePolicyReq where
  fromMultipart form = do
    UpsertFarePolicyReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp UpsertFarePolicyReq where
  toMultipart form =
    MultipartData [] [FileData "file" (T.pack form.file) "" (form.file)]

-- Update Onboarding Vehicle Variant Mapping

instance FromMultipart Tmp UpdateOnboardingVehicleVariantMappingReq where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    let vehicleCategory = lookupInput "vehicleCategory" form
    pure $
      UpdateOnboardingVehicleVariantMappingReq
        { file = fdPayload fileData,
          vehicleCategory = case vehicleCategory of
            Left _ -> "CAR" -- Having Default as CAR to support backward compatibility
            Right x -> x
        }

instance ToMultipart Tmp UpdateOnboardingVehicleVariantMappingReq where
  toMultipart form =
    MultipartData
      [Input "vehicleCategory" form.vehicleCategory]
      [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp UpsertMerchantPushNotificationCsvReq where
  fromMultipart form =
    UpsertMerchantPushNotificationCsvReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> lookupInput "merchantOperatingCity" form
      <*> lookupInput "merchantId" form

instance ToMultipart Tmp UpsertMerchantPushNotificationCsvReq where
  toMultipart form =
    MultipartData
      [Input "merchantOperatingCity" form.merchantOperatingCity, Input "merchantId" form.merchantId]
      [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp UpsertMerchantOverlayCsvReq where
  fromMultipart form =
    UpsertMerchantOverlayCsvReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> lookupInput "merchantOperatingCity" form
      <*> lookupInput "merchantId" form

instance ToMultipart Tmp UpsertMerchantOverlayCsvReq where
  toMultipart form =
    MultipartData
      [Input "merchantOperatingCity" form.merchantOperatingCity, Input "merchantId" form.merchantId]
      [FileData "file" (T.pack form.file) "" (form.file)]

instance FromMultipart Tmp UpsertMerchantMessageCsvReq where
  fromMultipart form =
    UpsertMerchantMessageCsvReq
      <$> fmap fdPayload (lookupFile "file" form)
      <*> lookupInput "merchantOperatingCity" form
      <*> lookupInput "merchantId" form

instance ToMultipart Tmp UpsertMerchantMessageCsvReq where
  toMultipart form =
    MultipartData
      [Input "merchantOperatingCity" form.merchantOperatingCity, Input "merchantId" form.merchantId]
      [FileData "file" (T.pack form.file) "" (form.file)]
