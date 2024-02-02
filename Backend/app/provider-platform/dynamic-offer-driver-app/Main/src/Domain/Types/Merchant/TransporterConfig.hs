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

import Data.Aeson as A
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.Text as Text
import Domain.Types.Common
import Domain.Types.Location (DummyLocationInfo, dummyToLocationData)
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import EulerHS.Prelude hiding (id)
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language)
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id

-- import  Data.ByteString.Lazy as LB

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

readWithInfo :: (Read a, Show a) => Value -> a
readWithInfo s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ Text.unpack str
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ show scientific
  _ -> error $ "Not able to parse value" <> show s

readWithInfo' :: (Read a, Show a) => Value -> Maybe a
readWithInfo' s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> Just val
    Nothing -> Nothing
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> Just val
    Nothing -> Nothing
  _ -> error $ "Not able to parse value" <> show s

parseFCMConfig :: Object -> Parser FCMConfig
parseFCMConfig v = do
  url <- v .: DAK.fromText (Text.pack "fcmUrl")
  serviceAccount <- v .: DAK.fromText (Text.pack "fcmServiceAccount")
  tokenKeyPrefix <- v .: DAK.fromText (Text.pack "fcmTokenKeyPrefix")
  return $ FCM.FCMConfig url serviceAccount tokenKeyPrefix

valueToMaybe :: FromJSON a => A.Value -> Maybe a
valueToMaybe value = case A.fromJSON value of
  A.Success a -> Just a
  A.Error _ -> Nothing

valueToText :: Value -> Text
valueToText val = case val of
  String text -> text
  _ -> error "Not a string"

valueToTextList :: Value -> [Text]
valueToTextList val =
  case val of
    String text -> KP.map strip . splitOn "," $ text
    _ -> error $ "Not a string or array" <> (show val)

jsonToTransporterConfig :: Object -> Parser TransporterConfig
jsonToTransporterConfig v =
  TransporterConfig
    <$> (Id <$> (v .: DAK.fromText (Text.pack "transporterConfig:merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "transporterConfig:merchantOperatingCityId")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:pickupLocThreshold")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:dropLocThreshold")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:rideTimeEstimatedThreshold")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:includeDriverCurrentlyOnRide")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:defaultPopupDelay")))
    <*> ((readWithInfo' :: (Value -> Maybe Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:popupDelayToAddAsPenalty")))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:thresholdCancellationScore")))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:minRidesForCancellationScore")))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:thresholdCancellationPercentageToUnlist")))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:minRidesToUnlist")))
    <*> ((valueToText :: (Value -> Text)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:mediaFileUrlPattern")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:mediaFileSizeUpperLimit")))
    <*> ((valueToText :: (Value -> Text)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:referralLinkPassword")))
    <*> (parseFCMConfig v)
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:onboardingTryLimit")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:onboardingRetryTimeInHours")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:checkImageExtractionForDashboard")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:searchRepeatLimit")))
    <*> ((readWithInfo :: (Value -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:actualRideDistanceDiffThreshold")))
    <*> ((readWithInfo :: (Value -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:upwardsRecomputeBuffer")))
    <*> ((readWithInfo :: (Value -> HighPrecMeters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:approxRideDistanceDiffThreshold")))
    <*> ((readWithInfo :: (Value -> Double)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:minLocationAccuracy")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverPaymentCycleBuffer")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverPaymentCycleDuration")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverPaymentCycleStartTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverPaymentReminderInterval")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverAutoPayNotificationTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverAutoPayExecutionTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:bankErrorExpiry")))))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeMandateNotificationBatchSize")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeMandateExecutionBatchSize")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:mandateNotificationRescheduleInterval")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:mandateExecutionRescheduleInterval")))))
    <*> (((fromIntegral :: (Int -> NominalDiffTime)) <$>) <$> (((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeCalculationTime")))))
    <*> ((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeCalculatorBatchSize")))
    <*> (((fromIntegral :: (Int -> NominalDiffTime)) <$>) <$> (((readWithInfo' :: (Value -> Maybe Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeCalculatorBatchGap")))))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeRetryThresholdConfig")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:orderAndNotificationStatusCheckTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:orderAndNotificationStatusCheckTimeLimit")))))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:badDebtBatchSize")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:badDebtRescheduleTime")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:badDebtSchedulerTime")))))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:badDebtTimeThreshold")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:timeDiffFromUtc")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:subscription")))
    <*> ((readWithInfo :: (Value -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:subscriptionStartTime")))
    <*> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:avgSpeedOfVehicle"))) :: Parser (Maybe AvgSpeedOfVechilePerKm))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:updateNotificationStatusBatchSize")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:updateOrderStatusBatchSize")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:mandateValidity")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:aadhaarVerificationRequired")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:enableDashboardSms")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverLocationAccuracyBuffer")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:routeDeviationThreshold")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:canDowngradeToSedan")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:canDowngradeToHatchback")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:canDowngradeToTaxi")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:canSuvDowngradeToTaxi")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:rcLimit")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:automaticRCActivationCutOff")))
    <*> ((readWithInfo :: (Value -> [Language])) <$> (v .: DAK.fromText (Text.pack "transporterConfig:languagesToBeTranslated")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:isAvoidToll")))
    <*> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:aadhaarImageResizeConfig"))) :: Parser (Maybe AadhaarImageResizeConfig))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:enableFaceVerification")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:specialZoneBookingOtpExpiry")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:isPlanMandatory")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:freeTrialDays")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:openMarketUnBlocked")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:cacheOfferListByDriverId")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:useOfferListCache")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:ratingAsDecimal")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:coinFeature")))
    <*> ((readWithInfo :: (Value -> HighPrecMoney)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:coinConversionRate")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:refillVehicleModel")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverFeeOverlaySendingTimeLimitInDays")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:overlayBatchSize")))
    <*> ((readWithInfo :: (Value -> Double)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:snapToRoadConfidenceThreshold")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:useWithSnapToRoadFallback")))
    <*> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:volunteerSmsSendingLimit"))) :: Parser (Maybe DashboardMediaSendingLimit))
    <*> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverSmsReceivingLimit"))) :: Parser (Maybe DashboardMediaSendingLimit))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:cancellationTimeDiff")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:coinExpireTime")))))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:stepFunctionToConvertCoins")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:cancellationDistDiff")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:considerSpecialZoneRidesForPlanCharges")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:considerSpecialZoneRideChargesInFreeTrial")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:enableUdfForOffers")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:nightSafetyRouteDeviationThreshold")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:nightSafetyStartTime")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:nightSafetyEndTime")))
    <*> ((readWithInfo :: (Value -> HighPrecMoney)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:cancellationFee")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverDistanceTravelledOnPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (Value -> Seconds)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverTimeSpentOnPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:cancellationFeeDisputeLimit")))
    <*> ((readWithInfo :: (Value -> Meters)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverDistanceToPickupThresholdOnCancel")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:numOfCancellationsAllowed")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:canAddCancellationFee")))
    <*> ((readWithInfo :: (Value -> Bool)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:allowDefaultPlanAllocation")))
    <*> ((valueToTextList :: (Value -> [Text])) <$> (v .: DAK.fromText (Text.pack "transporterConfig:specialDrivers")))
    <*> ((valueToTextList :: (Value -> [Text])) <$> (v .: DAK.fromText (Text.pack "transporterConfig:specialLocationTags")))
    <*> ((readWithInfo :: (Value -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:createdAt")))
    <*> ((readWithInfo :: (Value -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:updatedAt")))
    <*> ((valueToTextList :: (Value -> [Text])) <$> (v .: DAK.fromText (Text.pack "transporterConfig:notificationRetryEligibleErrorCodes")))
    <*> ((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:notificationRetryCountThreshold")))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:notificationRetryTimeGap")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:driverAutoPayExecutionTimeFallBack")))))
    <*> ((fromIntegral :: (Int -> NominalDiffTime)) <$> (((readWithInfo :: (Value -> Int)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:orderAndNotificationStatusCheckFallBackTime")))))
    <*> ((valueToText :: (Value -> Text)) <$> (v .: DAK.fromText (Text.pack "transporterConfig:kaptureDisposition")))
    <*> ((fromMaybe dummyToLocationData) <$> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:dummyFromLocation"))) :: Parser (Maybe DummyLocationInfo)))
    <*> ((fromMaybe dummyToLocationData) <$> ((valueToMaybe <$> (v .: DAK.fromText (Text.pack "transporterConfig:dummyToLocation"))) :: Parser (Maybe DummyLocationInfo)))
