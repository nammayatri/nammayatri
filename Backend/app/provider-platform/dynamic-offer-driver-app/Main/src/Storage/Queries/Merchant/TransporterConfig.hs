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
import Domain.Types.Merchant
import Domain.Types.Merchant.TransporterConfig
import Kernel.Beam.Functions
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.TransporterConfig as BeamTC

findByMerchantId :: MonadFlow m => Id Merchant -> m (Maybe TransporterConfig)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamTC.merchantId $ Se.Eq merchantId]

updateFCMConfig :: MonadFlow m => Id Merchant -> BaseUrl -> Text -> m ()
updateFCMConfig (Id merchantId) fcmUrl fcmServiceAccount = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.fcmUrl $ showBaseUrl fcmUrl,
      Se.Set BeamTC.fcmServiceAccount fcmServiceAccount,
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantId (Se.Eq merchantId)]

updateReferralLinkPassword :: MonadFlow m => Id Merchant -> Text -> m ()
updateReferralLinkPassword (Id merchantId) newPassword = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.referralLinkPassword newPassword,
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantId (Se.Eq merchantId)]

update :: MonadFlow m => TransporterConfig -> m ()
update config = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamTC.pickupLocThreshold config.pickupLocThreshold,
      Se.Set BeamTC.dropLocThreshold config.dropLocThreshold,
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
      Se.Set BeamTC.updatedAt now
    ]
    [Se.Is BeamTC.merchantId (Se.Eq $ getId config.merchantId)]

instance FromTType' BeamTC.TransporterConfig TransporterConfig where
  fromTType' BeamTC.TransporterConfigT {..} = do
    fcmUrl' <- parseBaseUrl fcmUrl
    pure $
      Just
        TransporterConfig
          { merchantId = Id merchantId,
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
            aadhaarImageResizeConfig = valueToMaybe =<< aadhaarImageResizeConfig,
            mandateNotificationRescheduleInterval = secondsToNominalDiffTime mandateNotificationRescheduleInterval,
            mandateExecutionRescheduleInterval = secondsToNominalDiffTime mandateExecutionRescheduleInterval,
            bankErrorExpiry = secondsToNominalDiffTime bankErrorExpiry,
            driverFeeCalculationTime = secondsToNominalDiffTime <$> driverFeeCalculationTime,
            driverFeeCalculatorBatchGap = secondsToNominalDiffTime <$> driverFeeCalculatorBatchGap,
            ..
          }
    where
      valueToMaybe :: FromJSON a => A.Value -> Maybe a
      valueToMaybe value = case A.fromJSON value of
        A.Success a -> Just a
        A.Error _ -> Nothing

instance ToTType' BeamTC.TransporterConfig TransporterConfig where
  toTType' TransporterConfig {..} = do
    BeamTC.TransporterConfigT
      { BeamTC.merchantId = getId merchantId,
        BeamTC.pickupLocThreshold = pickupLocThreshold,
        BeamTC.dropLocThreshold = dropLocThreshold,
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
        BeamTC.actualRideDistanceDiffThreshold = actualRideDistanceDiffThreshold,
        BeamTC.upwardsRecomputeBuffer = upwardsRecomputeBuffer,
        BeamTC.approxRideDistanceDiffThreshold = approxRideDistanceDiffThreshold,
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
        BeamTC.driverFeeCalculationTime = nominalDiffTimeToSeconds <$> driverFeeCalculationTime,
        BeamTC.driverFeeCalculatorBatchSize = driverFeeCalculatorBatchSize,
        BeamTC.driverFeeCalculatorBatchGap = nominalDiffTimeToSeconds <$> driverFeeCalculatorBatchGap,
        BeamTC.subscription = subscription,
        BeamTC.minLocationAccuracy = minLocationAccuracy,
        BeamTC.aadhaarVerificationRequired = aadhaarVerificationRequired,
        BeamTC.enableDashboardSms = enableDashboardSms,
        BeamTC.subscriptionStartTime = subscriptionStartTime,
        BeamTC.bankErrorExpiry = nominalDiffTimeToSeconds bankErrorExpiry,
        BeamTC.createdAt = createdAt,
        BeamTC.updatedAt = updatedAt,
        BeamTC.rcLimit = rcLimit,
        BeamTC.mandateValidity = mandateValidity,
        BeamTC.driverLocationAccuracyBuffer = driverLocationAccuracyBuffer,
        BeamTC.routeDeviationThreshold = routeDeviationThreshold,
        BeamTC.automaticRCActivationCutOff = automaticRCActivationCutOff,
        BeamTC.canDowngradeToSedan = canDowngradeToSedan,
        BeamTC.canDowngradeToHatchback = canDowngradeToHatchback,
        BeamTC.canDowngradeToTaxi = canDowngradeToTaxi,
        BeamTC.aadhaarImageResizeConfig = toJSON <$> aadhaarImageResizeConfig,
        BeamTC.isAvoidToll = isAvoidToll,
        BeamTC.specialZoneBookingOtpExpiry = specialZoneBookingOtpExpiry
      }
