{-# LANGUAGE InstanceSigs #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import qualified Data.Aeson as A
import Domain.Types.Location (dummyFromLocationData, dummyToLocationData)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import Kernel.Beam.Functions
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.TransporterConfig as BeamTC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => TransporterConfig -> m ()
create = createWithKV

findByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m (Maybe TransporterConfig)
findByMerchantOpCityId (Id merchantOperatingCityId) = findOneWithKV [Se.Is BeamTC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

updateFCMConfig :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> BaseUrl -> Text -> m ()
updateFCMConfig (Id merchantOperatingCityId) fcmUrl fcmServiceAccount = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.fcmUrl $ showBaseUrl fcmUrl,
      Se.Set BeamTC.fcmServiceAccount fcmServiceAccount,
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantOperatingCityId (Se.Eq merchantOperatingCityId)]

updateReferralLinkPassword :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Text -> m ()
updateReferralLinkPassword (Id merchantOperatingCityId) newPassword = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.referralLinkPassword newPassword,
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantOperatingCityId (Se.Eq merchantOperatingCityId)]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => TransporterConfig -> m ()
update config = do
  let distanceUnit = Just config.pickupLocThreshold.unit -- should be the same for all fields
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.pickupLocThreshold $ distanceToMeters config.pickupLocThreshold,
      Se.Set BeamTC.dropLocThreshold $ distanceToMeters config.dropLocThreshold,
      Se.Set BeamTC.pickupLocThresholdValue $ Just $ distanceToHighPrecDistance distanceUnit config.pickupLocThreshold,
      Se.Set BeamTC.dropLocThresholdValue $ Just $ distanceToHighPrecDistance distanceUnit config.dropLocThreshold,
      Se.Set BeamTC.rideTimeEstimatedThreshold config.rideTimeEstimatedThreshold,
      Se.Set BeamTC.defaultPopupDelay config.defaultPopupDelay,
      Se.Set BeamTC.popupDelayToAddAsPenalty config.popupDelayToAddAsPenalty,
      Se.Set BeamTC.thresholdCancellationScore config.thresholdCancellationScore,
      Se.Set BeamTC.minRidesForCancellationScore config.minRidesForCancellationScore,
      Se.Set BeamTC.mediaFileUrlPattern config.mediaFileUrlPattern,
      Se.Set BeamTC.mediaFileSizeUpperLimit config.mediaFileSizeUpperLimit,
      Se.Set BeamTC.onboardingTryLimit config.onboardingTryLimit,
      Se.Set BeamTC.onboardingRetryTimeInHours config.onboardingRetryTimeInHours,
      Se.Set BeamTC.checkImageExtractionForDashboard config.checkImageExtractionForDashboard,
      Se.Set BeamTC.searchRepeatLimit config.searchRepeatLimit,
      Se.Set BeamTC.driverPaymentCycleStartTime (nominalDiffTimeToSeconds config.driverPaymentCycleStartTime),
      Se.Set BeamTC.timeDiffFromUtc config.timeDiffFromUtc,
      Se.Set BeamTC.driverPaymentCycleBuffer (nominalDiffTimeToSeconds config.driverPaymentCycleBuffer),
      Se.Set BeamTC.driverPaymentReminderInterval (nominalDiffTimeToSeconds config.driverPaymentReminderInterval),
      Se.Set BeamTC.driverPaymentCycleDuration (nominalDiffTimeToSeconds config.driverPaymentCycleDuration),
      Se.Set BeamTC.driverAutoPayNotificationTime (nominalDiffTimeToSeconds config.driverAutoPayNotificationTime),
      Se.Set BeamTC.driverAutoPayExecutionTime (nominalDiffTimeToSeconds config.driverAutoPayExecutionTime),
      Se.Set BeamTC.driverFeeMandateNotificationBatchSize config.driverFeeMandateNotificationBatchSize,
      Se.Set BeamTC.driverFeeMandateExecutionBatchSize config.driverFeeMandateExecutionBatchSize,
      Se.Set BeamTC.mandateNotificationRescheduleInterval (nominalDiffTimeToSeconds config.mandateNotificationRescheduleInterval),
      Se.Set BeamTC.mandateExecutionRescheduleInterval (nominalDiffTimeToSeconds config.mandateExecutionRescheduleInterval),
      Se.Set BeamTC.driverFeeCalculationTime (nominalDiffTimeToSeconds <$> config.driverFeeCalculationTime),
      Se.Set BeamTC.driverFeeCalculatorBatchSize config.driverFeeCalculatorBatchSize,
      Se.Set BeamTC.driverFeeCalculatorBatchGap (nominalDiffTimeToSeconds <$> config.driverFeeCalculatorBatchGap),
      Se.Set BeamTC.orderAndNotificationStatusCheckTime (nominalDiffTimeToSeconds config.orderAndNotificationStatusCheckTime),
      Se.Set BeamTC.orderAndNotificationStatusCheckTimeLimit (nominalDiffTimeToSeconds config.orderAndNotificationStatusCheckTimeLimit),
      Se.Set BeamTC.snapToRoadConfidenceThreshold config.snapToRoadConfidenceThreshold,
      Se.Set BeamTC.useWithSnapToRoadFallback config.useWithSnapToRoadFallback,
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantOperatingCityId (Se.Eq $ getId config.merchantOperatingCityId)]

instance FromTType' BeamTC.TransporterConfig TransporterConfig where
  fromTType' BeamTC.TransporterConfigT {..} = do
    fcmUrl' <- parseBaseUrl fcmUrl
    pure $
      Just
        TransporterConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            fcmConfig =
              FCM.FCMConfig
                { fcmUrl = fcmUrl',
                  fcmServiceAccount = fcmServiceAccount,
                  fcmTokenKeyPrefix = fcmTokenKeyPrefix
                },
            driverPaymentCycleBuffer = secondsToNominalDiffTime driverPaymentCycleBuffer,
            driverPaymentCycleDuration = secondsToNominalDiffTime driverPaymentCycleDuration,
            driverPaymentCycleStartTime = secondsToNominalDiffTime driverPaymentCycleStartTime,
            driverPaymentReminderInterval = secondsToNominalDiffTime driverPaymentReminderInterval,
            driverAutoPayNotificationTime = secondsToNominalDiffTime driverAutoPayNotificationTime,
            driverAutoPayExecutionTime = secondsToNominalDiffTime driverAutoPayExecutionTime,
            languagesToBeTranslated,
            avgSpeedOfVehicle = valueToMaybe =<< avgSpeedOfVehicle,
            aadhaarImageResizeConfig = valueToMaybe =<< aadhaarImageResizeConfig,
            mandateNotificationRescheduleInterval = secondsToNominalDiffTime mandateNotificationRescheduleInterval,
            mandateExecutionRescheduleInterval = secondsToNominalDiffTime mandateExecutionRescheduleInterval,
            bankErrorExpiry = secondsToNominalDiffTime bankErrorExpiry,
            driverFeeCalculationTime = secondsToNominalDiffTime <$> driverFeeCalculationTime,
            driverFeeCalculatorBatchGap = secondsToNominalDiffTime <$> driverFeeCalculatorBatchGap,
            updateNotificationStatusBatchSize,
            updateOrderStatusBatchSize,
            orderAndNotificationStatusCheckTime = secondsToNominalDiffTime orderAndNotificationStatusCheckTime,
            orderAndNotificationStatusCheckTimeLimit = secondsToNominalDiffTime orderAndNotificationStatusCheckTimeLimit,
            volunteerSmsSendingLimit = valueToMaybe =<< volunteerSmsSendingLimit,
            driverSmsReceivingLimit = valueToMaybe =<< driverSmsReceivingLimit,
            coinFeature = coinFeature,
            coinConversionRate = coinConversionRate,
            cancellationTimeDiff = secondsToNominalDiffTime cancellationTimeDiff,
            coinExpireTime = secondsToNominalDiffTime coinExpireTime,
            stepFunctionToConvertCoins = stepFunctionToConvertCoins,
            cancellationDistDiff = cancellationDistDiff,
            notificationRetryTimeGap = secondsToNominalDiffTime notificationRetryTimeGap,
            badDebtRescheduleTime = secondsToNominalDiffTime badDebtRescheduleTime,
            badDebtSchedulerTime = secondsToNominalDiffTime badDebtSchedulerTime,
            badDebtTimeThreshold = badDebtTimeThreshold,
            driverAutoPayExecutionTimeFallBack = secondsToNominalDiffTime driverAutoPayExecutionTimeFallBack,
            orderAndNotificationStatusCheckFallBackTime = secondsToNominalDiffTime orderAndNotificationStatusCheckFallBackTime,
            dummyFromLocation = fromMaybe dummyFromLocationData (valueToMaybe =<< dummyFromLocation),
            dummyToLocation = fromMaybe dummyToLocationData (valueToMaybe =<< dummyToLocation),
            scheduleRideBufferTime = secondsToNominalDiffTime scheduleRideBufferTime,
            canSuvDowngradeToHatchback = fromMaybe False canSuvDowngradeToHatchback,
            arrivedPickupThreshold = fromMaybe 100 $ mkDistanceWithDefault distanceUnit arrivedPickupThresholdValue <$> arrivedPickupThreshold,
            variantsToEnableForSubscription = variantsToEnableForSubscription,
            pickupLocThreshold = mkDistanceWithDefaultMeters distanceUnit pickupLocThresholdValue pickupLocThreshold,
            dropLocThreshold = mkDistanceWithDefaultMeters distanceUnit dropLocThresholdValue dropLocThreshold,
            actualRideDistanceDiffThreshold = mkDistanceWithDefault distanceUnit actualRideDistanceDiffThresholdValue actualRideDistanceDiffThreshold,
            actualRideDistanceDiffThresholdIfWithinPickupDrop = mkDistanceWithDefault distanceUnit actualRideDistanceDiffThresholdIfWithinPickupDropValue actualRideDistanceDiffThresholdIfWithinPickupDrop,
            upwardsRecomputeBuffer = mkDistanceWithDefault distanceUnit upwardsRecomputeBufferValue upwardsRecomputeBuffer,
            approxRideDistanceDiffThreshold = mkDistanceWithDefault distanceUnit approxRideDistanceDiffThresholdValue approxRideDistanceDiffThreshold,
            driverLocationAccuracyBuffer = mkDistanceWithDefaultMeters distanceUnit driverLocationAccuracyBufferValue driverLocationAccuracyBuffer,
            routeDeviationThreshold = mkDistanceWithDefaultMeters distanceUnit routeDeviationThresholdValue routeDeviationThreshold,
            nightSafetyRouteDeviationThreshold = mkDistanceWithDefaultMeters distanceUnit nightSafetyRouteDeviationThresholdValue nightSafetyRouteDeviationThreshold,
            driverDistanceTravelledOnPickupThresholdOnCancel = mkDistanceWithDefaultMeters distanceUnit driverDistanceTravelledOnPickupThresholdOnCancelValue driverDistanceTravelledOnPickupThresholdOnCancel,
            driverDistanceToPickupThresholdOnCancel = mkDistanceWithDefaultMeters distanceUnit driverDistanceToPickupThresholdOnCancelValue driverDistanceToPickupThresholdOnCancel,
            arrivedStopThreshold = mkDistanceWithDefault distanceUnit arrivedStopThresholdValue <$> arrivedStopThreshold,
            ..
          }
    where
      valueToMaybe :: FromJSON a => A.Value -> Maybe a
      valueToMaybe value = case A.fromJSON value of
        A.Success a -> Just a
        A.Error _ -> Nothing

instance ToTType' BeamTC.TransporterConfig TransporterConfig where
  toTType' :: TransporterConfig -> BeamTC.TransporterConfig
  toTType' TransporterConfig {..} = do
    let distanceUnit = Just pickupLocThreshold.unit -- should be the same for all fields
    BeamTC.TransporterConfigT
      { BeamTC.merchantId = getId merchantId,
        BeamTC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamTC.pickupLocThreshold = distanceToMeters pickupLocThreshold,
        BeamTC.pickupLocThresholdValue = Just $ distanceToHighPrecDistance distanceUnit pickupLocThreshold,
        BeamTC.dropLocThresholdValue = Just $ distanceToHighPrecDistance distanceUnit dropLocThreshold,
        BeamTC.actualRideDistanceDiffThresholdValue = Just $ distanceToHighPrecDistance distanceUnit actualRideDistanceDiffThreshold,
        BeamTC.actualRideDistanceDiffThresholdIfWithinPickupDropValue = Just $ distanceToHighPrecDistance distanceUnit actualRideDistanceDiffThresholdIfWithinPickupDrop,
        BeamTC.upwardsRecomputeBufferValue = Just $ distanceToHighPrecDistance distanceUnit upwardsRecomputeBuffer,
        BeamTC.approxRideDistanceDiffThresholdValue = Just $ distanceToHighPrecDistance distanceUnit approxRideDistanceDiffThreshold,
        BeamTC.driverLocationAccuracyBufferValue = Just $ distanceToHighPrecDistance distanceUnit driverLocationAccuracyBuffer,
        BeamTC.routeDeviationThresholdValue = Just $ distanceToHighPrecDistance distanceUnit routeDeviationThreshold,
        BeamTC.nightSafetyRouteDeviationThresholdValue = Just $ distanceToHighPrecDistance distanceUnit nightSafetyRouteDeviationThreshold,
        BeamTC.driverDistanceTravelledOnPickupThresholdOnCancelValue = Just $ distanceToHighPrecDistance distanceUnit driverDistanceTravelledOnPickupThresholdOnCancel,
        BeamTC.driverDistanceToPickupThresholdOnCancelValue = Just $ distanceToHighPrecDistance distanceUnit driverDistanceToPickupThresholdOnCancel,
        BeamTC.arrivedStopThresholdValue = distanceToHighPrecDistance distanceUnit <$> arrivedStopThreshold,
        BeamTC.arrivedPickupThresholdValue = Just $ distanceToHighPrecDistance distanceUnit arrivedPickupThreshold,
        BeamTC.distanceUnit = distanceUnit,
        BeamTC.dropLocThreshold = distanceToMeters dropLocThreshold,
        BeamTC.rideTimeEstimatedThreshold = rideTimeEstimatedThreshold,
        BeamTC.includeDriverCurrentlyOnRide = includeDriverCurrentlyOnRide,
        BeamTC.defaultPopupDelay = defaultPopupDelay,
        BeamTC.popupDelayToAddAsPenalty = popupDelayToAddAsPenalty,
        BeamTC.thresholdCancellationScore = thresholdCancellationScore,
        BeamTC.minRidesForCancellationScore = minRidesForCancellationScore,
        BeamTC.thresholdCancellationPercentageToUnlist = thresholdCancellationPercentageToUnlist,
        BeamTC.minRidesToUnlist = minRidesToUnlist,
        BeamTC.mediaFileUrlPattern = mediaFileUrlPattern,
        BeamTC.mediaFileSizeUpperLimit = mediaFileSizeUpperLimit,
        BeamTC.referralLinkPassword = referralLinkPassword,
        BeamTC.fcmUrl = showBaseUrl $ FCM.fcmUrl fcmConfig,
        BeamTC.fcmServiceAccount = FCM.fcmServiceAccount fcmConfig,
        BeamTC.fcmTokenKeyPrefix = FCM.fcmTokenKeyPrefix fcmConfig,
        BeamTC.onboardingTryLimit = onboardingTryLimit,
        BeamTC.onboardingRetryTimeInHours = onboardingRetryTimeInHours,
        BeamTC.checkImageExtractionForDashboard = checkImageExtractionForDashboard,
        BeamTC.searchRepeatLimit = searchRepeatLimit,
        BeamTC.actualRideDistanceDiffThreshold = distanceToHighPrecMeters actualRideDistanceDiffThreshold,
        BeamTC.recomputeIfPickupDropNotOutsideOfThreshold = recomputeIfPickupDropNotOutsideOfThreshold,
        BeamTC.actualRideDistanceDiffThresholdIfWithinPickupDrop = distanceToHighPrecMeters actualRideDistanceDiffThresholdIfWithinPickupDrop,
        BeamTC.upwardsRecomputeBuffer = distanceToHighPrecMeters upwardsRecomputeBuffer,
        BeamTC.approxRideDistanceDiffThreshold = distanceToHighPrecMeters approxRideDistanceDiffThreshold,
        BeamTC.driverPaymentCycleBuffer = nominalDiffTimeToSeconds driverPaymentCycleBuffer,
        BeamTC.driverPaymentCycleDuration = nominalDiffTimeToSeconds driverPaymentCycleDuration,
        BeamTC.driverPaymentCycleStartTime = nominalDiffTimeToSeconds driverPaymentCycleStartTime,
        BeamTC.driverPaymentReminderInterval = nominalDiffTimeToSeconds driverPaymentReminderInterval,
        BeamTC.driverAutoPayNotificationTime = nominalDiffTimeToSeconds driverAutoPayNotificationTime,
        BeamTC.driverAutoPayExecutionTime = nominalDiffTimeToSeconds driverAutoPayExecutionTime,
        BeamTC.mandateNotificationRescheduleInterval = nominalDiffTimeToSeconds mandateNotificationRescheduleInterval,
        BeamTC.mandateExecutionRescheduleInterval = nominalDiffTimeToSeconds mandateExecutionRescheduleInterval,
        BeamTC.driverFeeMandateNotificationBatchSize = driverFeeMandateNotificationBatchSize,
        BeamTC.driverFeeMandateExecutionBatchSize = driverFeeMandateExecutionBatchSize,
        BeamTC.timeDiffFromUtc = timeDiffFromUtc,
        BeamTC.driverFeeRetryThresholdConfig = driverFeeRetryThresholdConfig,
        BeamTC.driverFeeCalculationTime = nominalDiffTimeToSeconds <$> driverFeeCalculationTime,
        BeamTC.driverFeeCalculatorBatchSize = driverFeeCalculatorBatchSize,
        BeamTC.driverFeeCalculatorBatchGap = nominalDiffTimeToSeconds <$> driverFeeCalculatorBatchGap,
        BeamTC.orderAndNotificationStatusCheckTime = nominalDiffTimeToSeconds orderAndNotificationStatusCheckTime,
        BeamTC.orderAndNotificationStatusCheckTimeLimit = nominalDiffTimeToSeconds orderAndNotificationStatusCheckTimeLimit,
        BeamTC.subscription = subscription,
        BeamTC.minLocationAccuracy = minLocationAccuracy,
        BeamTC.aadhaarVerificationRequired = aadhaarVerificationRequired,
        BeamTC.enableDashboardSms = enableDashboardSms,
        BeamTC.subscriptionStartTime = subscriptionStartTime,
        BeamTC.bankErrorExpiry = nominalDiffTimeToSeconds bankErrorExpiry,
        BeamTC.rcLimit = rcLimit,
        BeamTC.mandateValidity = mandateValidity,
        BeamTC.driverLocationAccuracyBuffer = distanceToMeters driverLocationAccuracyBuffer,
        BeamTC.routeDeviationThreshold = distanceToMeters routeDeviationThreshold,
        BeamTC.automaticRCActivationCutOff = automaticRCActivationCutOff,
        BeamTC.languagesToBeTranslated = languagesToBeTranslated,
        BeamTC.isPlanMandatory = isPlanMandatory,
        BeamTC.freeTrialDays = freeTrialDays,
        BeamTC.openMarketUnBlocked = openMarketUnBlocked,
        BeamTC.cacheOfferListByDriverId = cacheOfferListByDriverId,
        BeamTC.useOfferListCache = useOfferListCache,
        BeamTC.canDowngradeToSedan = canDowngradeToSedan,
        BeamTC.canDowngradeToHatchback = canDowngradeToHatchback,
        BeamTC.canSwitchToRental = canSwitchToRental,
        BeamTC.canSwitchToInterCity = canSwitchToInterCity,
        BeamTC.canDowngradeToTaxi = canDowngradeToTaxi,
        BeamTC.canSuvDowngradeToTaxi = canSuvDowngradeToTaxi,
        BeamTC.canSuvDowngradeToHatchback = Just canSuvDowngradeToHatchback,
        BeamTC.avgSpeedOfVehicle = toJSON <$> avgSpeedOfVehicle,
        BeamTC.aadhaarImageResizeConfig = toJSON <$> aadhaarImageResizeConfig,
        BeamTC.enableFaceVerification = enableFaceVerification,
        BeamTC.isAvoidToll = isAvoidToll,
        BeamTC.allowAutosOnTollRoute = allowAutosOnTollRoute,
        BeamTC.specialZoneBookingOtpExpiry = specialZoneBookingOtpExpiry,
        BeamTC.updateNotificationStatusBatchSize = updateNotificationStatusBatchSize,
        BeamTC.updateOrderStatusBatchSize = updateOrderStatusBatchSize,
        BeamTC.ratingAsDecimal = ratingAsDecimal,
        BeamTC.coinFeature = coinFeature,
        BeamTC.refillVehicleModel = refillVehicleModel,
        BeamTC.coinConversionRate = coinConversionRate,
        BeamTC.driverFeeOverlaySendingTimeLimitInDays = driverFeeOverlaySendingTimeLimitInDays,
        BeamTC.overlayBatchSize = overlayBatchSize,
        BeamTC.volunteerSmsSendingLimit = toJSON <$> volunteerSmsSendingLimit,
        BeamTC.driverSmsReceivingLimit = toJSON <$> driverSmsReceivingLimit,
        BeamTC.snapToRoadConfidenceThreshold = snapToRoadConfidenceThreshold,
        BeamTC.useWithSnapToRoadFallback = useWithSnapToRoadFallback,
        BeamTC.badDebtRescheduleTime = nominalDiffTimeToSeconds badDebtRescheduleTime,
        BeamTC.badDebtSchedulerTime = nominalDiffTimeToSeconds badDebtSchedulerTime,
        BeamTC.badDebtBatchSize = badDebtBatchSize,
        BeamTC.badDebtTimeThreshold = badDebtTimeThreshold,
        BeamTC.cancellationTimeDiff = nominalDiffTimeToSeconds cancellationTimeDiff,
        BeamTC.cancellationDistDiff = cancellationDistDiff,
        BeamTC.coinExpireTime = nominalDiffTimeToSeconds coinExpireTime,
        BeamTC.stepFunctionToConvertCoins = stepFunctionToConvertCoins,
        BeamTC.considerSpecialZoneRidesForPlanCharges = considerSpecialZoneRidesForPlanCharges,
        BeamTC.considerSpecialZoneRideChargesInFreeTrial = considerSpecialZoneRideChargesInFreeTrial,
        BeamTC.enableUdfForOffers = enableUdfForOffers,
        BeamTC.nightSafetyRouteDeviationThreshold = distanceToMeters nightSafetyRouteDeviationThreshold,
        BeamTC.nightSafetyStartTime = nightSafetyStartTime,
        BeamTC.nightSafetyEndTime = nightSafetyEndTime,
        BeamTC.cancellationFee = cancellationFee,
        BeamTC.driverDistanceTravelledOnPickupThresholdOnCancel = distanceToMeters driverDistanceTravelledOnPickupThresholdOnCancel,
        BeamTC.driverTimeSpentOnPickupThresholdOnCancel = driverTimeSpentOnPickupThresholdOnCancel,
        BeamTC.driverDistanceToPickupThresholdOnCancel = distanceToMeters driverDistanceToPickupThresholdOnCancel,
        BeamTC.cancellationFeeDisputeLimit = cancellationFeeDisputeLimit,
        BeamTC.numOfCancellationsAllowed = numOfCancellationsAllowed,
        BeamTC.canAddCancellationFee = canAddCancellationFee,
        BeamTC.allowDefaultPlanAllocation = allowDefaultPlanAllocation,
        BeamTC.notificationRetryEligibleErrorCodes = notificationRetryEligibleErrorCodes,
        BeamTC.notificationRetryCountThreshold = notificationRetryCountThreshold,
        BeamTC.notificationRetryTimeGap = nominalDiffTimeToSeconds notificationRetryTimeGap,
        BeamTC.driverAutoPayExecutionTimeFallBack = nominalDiffTimeToSeconds driverAutoPayExecutionTimeFallBack,
        BeamTC.orderAndNotificationStatusCheckFallBackTime = nominalDiffTimeToSeconds orderAndNotificationStatusCheckFallBackTime,
        BeamTC.bookAnyVehicleDowngradeLevel = bookAnyVehicleDowngradeLevel,
        BeamTC.scheduleRideBufferTime = nominalDiffTimeToSeconds scheduleRideBufferTime,
        BeamTC.considerDriversForSearch = considerDriversForSearch,
        BeamTC.arrivedStopThreshold = distanceToHighPrecMeters <$> arrivedStopThreshold,
        BeamTC.arrivedPickupThreshold = Just $ distanceToHighPrecMeters arrivedPickupThreshold,
        BeamTC.createdAt = createdAt,
        BeamTC.updatedAt = updatedAt,
        BeamTC.specialDrivers = specialDrivers,
        BeamTC.specialLocationTags = specialLocationTags,
        BeamTC.kaptureDisposition = kaptureDisposition,
        BeamTC.fakeOtpMobileNumbers = fakeOtpMobileNumbers,
        BeamTC.dummyFromLocation = Just $ toJSON dummyFromLocation,
        BeamTC.dummyToLocation = Just $ toJSON dummyToLocation,
        BeamTC.variantsToEnableForSubscription = variantsToEnableForSubscription,
        BeamTC.dlNumberVerification = dlNumberVerification,
        BeamTC.pastDaysRideCounter = pastDaysRideCounter,
        BeamTC.placeNameCacheExpiryDays = placeNameCacheExpiryDays
      }
