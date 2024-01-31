{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.TransporterConfig where

-- import Data.Time (NominalDiffTime, UTCTime)

import Data.Aeson
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.Location (DummyLocationInfo)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import EulerHS.Prelude hiding (id)
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language)
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id

-- import Data.Time.Clock.POSIX

data AadhaarImageResizeConfig = AadhaarImageResizeConfig
  { height :: Int,
    width :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, Read)

data AvgSpeedOfVechilePerKm = AvgSpeedOfVechilePerKm -- FIXME make datatype to [(Variant, Kilometers)]
  { sedan :: Kilometers,
    suv :: Kilometers,
    hatchback :: Kilometers,
    autorickshaw :: Kilometers,
    taxi :: Kilometers,
    taxiplus :: Kilometers
  }
  deriving (Generic, Show, FromJSON, ToJSON, Read)

data DashboardMediaSendingLimit = DashboardMediaSendingLimit
  { sms :: Int,
    whatsapp :: Int,
    overlay :: Int,
    alert :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, Read)

-- ProviderConfig?
data TransporterConfigD u = TransporterConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    pickupLocThreshold :: Meters,
    dropLocThreshold :: Meters,
    rideTimeEstimatedThreshold :: Seconds,
    includeDriverCurrentlyOnRide :: Bool,
    defaultPopupDelay :: Seconds,
    popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int,
    thresholdCancellationPercentageToUnlist :: Maybe Int,
    minRidesToUnlist :: Maybe Int,
    mediaFileUrlPattern :: Text,
    mediaFileSizeUpperLimit :: Int,
    referralLinkPassword :: Text,
    fcmConfig :: FCMConfig,
    onboardingTryLimit :: Int,
    onboardingRetryTimeInHours :: Int,
    checkImageExtractionForDashboard :: Bool,
    searchRepeatLimit :: Int,
    actualRideDistanceDiffThreshold :: HighPrecMeters,
    upwardsRecomputeBuffer :: HighPrecMeters,
    approxRideDistanceDiffThreshold :: HighPrecMeters,
    minLocationAccuracy :: Double,
    driverPaymentCycleBuffer :: NominalDiffTime,
    driverPaymentCycleDuration :: NominalDiffTime,
    driverPaymentCycleStartTime :: NominalDiffTime,
    driverPaymentReminderInterval :: NominalDiffTime,
    driverAutoPayNotificationTime :: NominalDiffTime,
    driverAutoPayExecutionTime :: NominalDiffTime,
    bankErrorExpiry :: NominalDiffTime,
    driverFeeMandateNotificationBatchSize :: Int,
    driverFeeMandateExecutionBatchSize :: Int,
    mandateNotificationRescheduleInterval :: NominalDiffTime,
    mandateExecutionRescheduleInterval :: NominalDiffTime,
    driverFeeCalculationTime :: Maybe NominalDiffTime,
    driverFeeCalculatorBatchSize :: Maybe Int,
    driverFeeCalculatorBatchGap :: Maybe NominalDiffTime,
    driverFeeRetryThresholdConfig :: Int,
    orderAndNotificationStatusCheckTime :: NominalDiffTime,
    orderAndNotificationStatusCheckTimeLimit :: NominalDiffTime,
    badDebtBatchSize :: Int,
    badDebtRescheduleTime :: NominalDiffTime,
    badDebtSchedulerTime :: NominalDiffTime,
    badDebtTimeThreshold :: Int,
    timeDiffFromUtc :: Seconds,
    subscription :: Bool,
    subscriptionStartTime :: UTCTime,
    avgSpeedOfVehicle :: Maybe AvgSpeedOfVechilePerKm,
    updateNotificationStatusBatchSize :: Int,
    updateOrderStatusBatchSize :: Int,
    mandateValidity :: Int,
    aadhaarVerificationRequired :: Bool,
    enableDashboardSms :: Bool,
    driverLocationAccuracyBuffer :: Meters,
    routeDeviationThreshold :: Meters,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSuvDowngradeToTaxi :: Bool,
    rcLimit :: Int,
    automaticRCActivationCutOff :: Seconds,
    languagesToBeTranslated :: [Language],
    isAvoidToll :: Bool,
    aadhaarImageResizeConfig :: Maybe AadhaarImageResizeConfig,
    enableFaceVerification :: Bool,
    specialZoneBookingOtpExpiry :: Int,
    isPlanMandatory :: Bool,
    freeTrialDays :: Int,
    openMarketUnBlocked :: Bool,
    cacheOfferListByDriverId :: Bool,
    useOfferListCache :: Bool,
    ratingAsDecimal :: Bool,
    coinFeature :: Bool,
    coinConversionRate :: HighPrecMoney,
    refillVehicleModel :: Bool,
    driverFeeOverlaySendingTimeLimitInDays :: Int,
    overlayBatchSize :: Int,
    snapToRoadConfidenceThreshold :: Double,
    useWithSnapToRoadFallback :: Bool,
    volunteerSmsSendingLimit :: Maybe DashboardMediaSendingLimit,
    driverSmsReceivingLimit :: Maybe DashboardMediaSendingLimit,
    cancellationTimeDiff :: NominalDiffTime,
    coinExpireTime :: NominalDiffTime,
    stepFunctionToConvertCoins :: Int,
    cancellationDistDiff :: Int,
    considerSpecialZoneRidesForPlanCharges :: Bool,
    considerSpecialZoneRideChargesInFreeTrial :: Bool,
    enableUdfForOffers :: Bool,
    nightSafetyRouteDeviationThreshold :: Meters,
    nightSafetyStartTime :: Seconds,
    nightSafetyEndTime :: Seconds,
    cancellationFee :: HighPrecMoney,
    driverDistanceTravelledOnPickupThresholdOnCancel :: Meters,
    driverTimeSpentOnPickupThresholdOnCancel :: Seconds,
    cancellationFeeDisputeLimit :: Int,
    driverDistanceToPickupThresholdOnCancel :: Meters,
    numOfCancellationsAllowed :: Int,
    canAddCancellationFee :: Bool,
    allowDefaultPlanAllocation :: Bool,
    specialDrivers :: [Text],
    specialLocationTags :: [Text],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    notificationRetryEligibleErrorCodes :: [Text],
    notificationRetryCountThreshold :: Int,
    notificationRetryTimeGap :: NominalDiffTime,
    driverAutoPayExecutionTimeFallBack :: NominalDiffTime,
    orderAndNotificationStatusCheckFallBackTime :: NominalDiffTime,
    kaptureDisposition :: Text,
    dummyFromLocation :: DummyLocationInfo,
    dummyToLocation :: DummyLocationInfo
  }
  deriving (Generic, Show)

type TransporterConfig = TransporterConfigD 'Safe

instance FromJSON (TransporterConfigD 'Unsafe)

instance ToJSON (TransporterConfigD 'Unsafe)

readWithInfo :: (Read a, Show a) => String -> a
readWithInfo s = case KP.readMaybe s of
  Just val -> val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s

parseFCMConfig :: Object -> Parser FCMConfig
parseFCMConfig v = do
  url <- v .: DAK.fromText (Text.pack "fcmUrl")
  serviceAccount <- v .: DAK.fromText (Text.pack "fcmServiceAccount")
  tokenKeyPrefix <- v .: DAK.fromText (Text.pack "fcmTokenKeyPrefix")
  return $ FCM.FCMConfig url serviceAccount tokenKeyPrefix

jsonToTransporterConfig :: Object -> Parser TransporterConfig
jsonToTransporterConfig v =
  TransporterConfig
    <$> (Id <$> (v .: DAK.fromText (Text.pack "merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "merchantOperatingCityId")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "pickupLocThreshold")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "dropLocThreshold")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "rideTimeEstimatedThreshold")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "includeDriverCurrentlyOnRide")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "defaultPopupDelay")))
    <*> ((readWithInfo :: (String -> Maybe Seconds)) <$> (v .: DAK.fromText (Text.pack "popupDelayToAddAsPenalty")))
    <*> ((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "thresholdCancellationScore")))
    <*> ((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "minRidesForCancellationScore")))
    <*> ((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "thresholdCancellationPercentageToUnlist")))
    <*> ((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "minRidesToUnlist")))
    <*> ((readWithInfo :: (String -> Text)) <$> (v .: DAK.fromText (Text.pack "mediaFileUrlPattern")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "mediaFileSizeUpperLimit")))
    <*> ((readWithInfo :: (String -> Text)) <$> (v .: DAK.fromText (Text.pack "referralLinkPassword")))
    <*> (parseFCMConfig v)
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "onboardingTryLimit")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "onboardingRetryTimeInHours")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "checkImageExtractionForDashboard")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "searchRepeatLimit")))
    <*> ((readWithInfo :: (String -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "actualRideDistanceDiffThreshold")))
    <*> ((readWithInfo :: (String -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "upwardsRecomputeBuffer")))
    <*> ((readWithInfo :: (String -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "approxRideDistanceDiffThreshold")))
    <*> ((readWithInfo :: (String -> Double)) <$> (v .: DAK.fromText (Text.pack "minLocationAccuracy")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPaymentCycleBuffer")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPaymentCycleDuration")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPaymentCycleStartTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPaymentReminderInterval")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverAutoPayNotificationTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverAutoPayExecutionTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "bankErrorExpiry")))))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeMandateNotificationBatchSize")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeMandateExecutionBatchSize")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "mandateNotificationRescheduleInterval")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "mandateExecutionRescheduleInterval")))))
    <*> (((fromIntegral :: (Int -> NominalDiffTime)) <$>) <$> (((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeCalculationTime")))))
    <*> ((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeCalculatorBatchSize")))
    <*> (((fromIntegral :: (Int -> NominalDiffTime)) <$>) <$> (((readWithInfo :: (String -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeCalculatorBatchGap")))))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeRetryThresholdConfig")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "orderAndNotificationStatusCheckTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "orderAndNotificationStatusCheckTimeLimit")))))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "badDebtBatchSize")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "badDebtRescheduleTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "badDebtSchedulerTime")))))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "badDebtTimeThreshold")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "timeDiffFromUtc")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "subscription")))
    <*> ((readWithInfo :: (String -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "subscriptionStartTime")))
    <*> ((readWithInfo :: (String -> Maybe AvgSpeedOfVechilePerKm)) <$> (v .: DAK.fromText (Text.pack "avgSpeedOfVehicle")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "updateNotificationStatusBatchSize")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "updateOrderStatusBatchSize")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "mandateValidity")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "aadhaarVerificationRequired")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "enableDashboardSms")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "driverLocationAccuracyBuffer")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "routeDeviationThreshold")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "canDowngradeToSedan")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "canDowngradeToHatchback")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "canDowngradeToTaxi")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "canSuvDowngradeToTaxi")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "rcLimit")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "automaticRCActivationCutOff")))
    <*> ((readWithInfo :: (String -> [Language])) <$> (v .: DAK.fromText (Text.pack "languagesToBeTranslated")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "isAvoidToll")))
    <*> ((readWithInfo :: (String -> Maybe AadhaarImageResizeConfig)) <$> (v .: DAK.fromText (Text.pack "aadhaarImageResizeConfig")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "enableFaceVerification")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "specialZoneBookingOtpExpiry")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "isPlanMandatory")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "freeTrialDays")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "openMarketUnBlocked")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "cacheOfferListByDriverId")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "useOfferListCache")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "ratingAsDecimal")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "coinFeature")))
    <*> ((readWithInfo :: (String -> HighPrecMoney)) <$> (v .: DAK.fromText (Text.pack "coinConversionRate")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "refillVehicleModel")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverFeeOverlaySendingTimeLimitInDays")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "overlayBatchSize")))
    <*> ((readWithInfo :: (String -> Double)) <$> (v .: DAK.fromText (Text.pack "snapToRoadConfidenceThreshold")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "useWithSnapToRoadFallback")))
    <*> ((readWithInfo :: (String -> Maybe DashboardMediaSendingLimit)) <$> (v .: DAK.fromText (Text.pack "volunteerSmsSendingLimit")))
    <*> ((readWithInfo :: (String -> Maybe DashboardMediaSendingLimit)) <$> (v .: DAK.fromText (Text.pack "driverSmsReceivingLimit")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "cancellationTimeDiff")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "coinExpireTime")))))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "stepFunctionToConvertCoins")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "cancellationDistDiff")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "considerSpecialZoneRidesForPlanCharges")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "considerSpecialZoneRideChargesInFreeTrial")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "enableUdfForOffers")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "nightSafetyRouteDeviationThreshold")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "nightSafetyStartTime")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "nightSafetyEndTime")))
    <*> ((readWithInfo :: (String -> HighPrecMoney)) <$> (v .: DAK.fromText (Text.pack "cancellationFee")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "driverDistanceTravelledOnPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (String -> Seconds)) <$> (v .: DAK.fromText (Text.pack "driverTimeSpentOnPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "cancellationFeeDisputeLimit")))
    <*> ((readWithInfo :: (String -> Meters)) <$> (v .: DAK.fromText (Text.pack "driverDistanceToPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "numOfCancellationsAllowed")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "canAddCancellationFee")))
    <*> ((readWithInfo :: (String -> Bool)) <$> (v .: DAK.fromText (Text.pack "allowDefaultPlanAllocation")))
    <*> ((readWithInfo :: (String -> [Text])) <$> (v .: DAK.fromText (Text.pack "specialDrivers")))
    <*> ((readWithInfo :: (String -> [Text])) <$> (v .: DAK.fromText (Text.pack "specialLocationTags")))
    <*> ((readWithInfo :: (String -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "createdAt")))
    <*> ((readWithInfo :: (String -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "updatedAt")))
    <*> ((readWithInfo :: (String -> [Text])) <$> (v .: DAK.fromText (Text.pack "notificationRetryEligibleErrorCodes")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "notificationRetryCountThreshold")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "notificationRetryTimeGap")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverAutoPayExecutionTimeFallBack")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "orderAndNotificationStatusCheckFallBackTime")))))
    <*> ((readWithInfo :: (String -> Text)) <$> (v .: DAK.fromText (Text.pack "kaptureDisposition")))
    <*> ((readWithInfo :: (String -> DummyLocationInfo)) <$> (v .: DAK.fromText (Text.pack "dummyFromLocation")))
    <*> ((readWithInfo :: (String -> DummyLocationInfo)) <$> (v .: DAK.fromText (Text.pack "dummyToLocation")))
