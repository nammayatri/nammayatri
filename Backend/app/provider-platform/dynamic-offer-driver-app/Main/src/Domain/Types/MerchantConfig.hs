{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.MerchantConfig where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import Domain.Types.Common
import Domain.Types.Merchant
import EulerHS.Prelude hiding (id)
import Kernel.External.Notification.FCM.Types (FCMConfig)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Geofencing
import Kernel.Types.Id

data MerchantConfigD (s :: UsageSafety) = MerchantConfig
  { merchantId :: Id Merchant,
    gstin :: Maybe Text,
    name :: Text,
    description :: Maybe Text,
    city :: Context.City,
    country :: Context.Country,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text,
    fromTime :: Maybe UTCTime,
    toTime :: Maybe UTCTime,
    headCount :: Maybe Int,
    status :: Status,
    verified :: Bool,
    enabled :: Bool,
    internalApiKey :: Text,
    geoHashPrecisionValue :: Int,
    geofencingConfig :: GeofencingConfig,
    info :: Maybe Text,
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
    timeDiffFromUtc :: Seconds,
    subscription :: Bool,
    subscriptionStartTime :: UTCTime,
    mandateValidity :: Int,
    aadhaarVerificationRequired :: Bool,
    minimumDriverRatesCount :: Int,
    enableDashboardSms :: Bool,
    driverLocationAccuracyBuffer :: Meters,
    routeDeviationThreshold :: Meters,
    rcLimit :: Int,
    automaticRCActivationCutOff :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    registryUrl :: BaseUrl
  }
  deriving (Generic, Show)

type MerchantConfig = MerchantConfigD 'Safe

instance FromJSON (MerchantConfigD 'Unsafe)

instance ToJSON (MerchantConfigD 'Unsafe)

data MerchantConfigAPIEntity = MerchantConfigAPIEntity
  { id :: Id Merchant,
    name :: Text,
    description :: Maybe Text,
    contactNumber :: Text,
    status :: Status,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

makeMerchantConfigAPIEntity :: MerchantConfig -> MerchantConfigAPIEntity
makeMerchantConfigAPIEntity MerchantConfig {..} =
  MerchantConfigAPIEntity
    { id = merchantId,
      contactNumber = fromMaybe "Unknown" $ mobileCountryCode <> mobileNumber,
      ..
    }
