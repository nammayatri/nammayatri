{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Storage.Beam.Merchant.MerchantConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Geofencing
import Lib.Utils ()
import Sequelize

data MerchantConfigT f = MerchantConfigT
  { merchantId :: B.C f Text,
    gstin :: B.C f (Maybe Text),
    name :: B.C f Text,
    verified :: B.C f Bool,
    enabled :: B.C f Bool,
    description :: B.C f (Maybe Text),
    mobileNumber :: B.C f (Maybe Text),
    mobileCountryCode :: B.C f (Maybe Text),
    fromTime :: B.C f (Maybe Time.UTCTime),
    toTime :: B.C f (Maybe Time.UTCTime),
    internalApiKey :: B.C f Text,
    headCount :: B.C f (Maybe Int),
    status :: B.C f Domain.Status,
    info :: B.C f (Maybe Text),
    originRestriction :: B.C f GeoRestriction,
    destinationRestriction :: B.C f GeoRestriction,
    city :: B.C f Context.City,
    country :: B.C f Context.Country,
    geoHashPrecisionValue :: B.C f Int,
    minimumDriverRatesCount :: B.C f Int,
    pickupLocThreshold :: B.C f Meters,
    dropLocThreshold :: B.C f Meters,
    rideTimeEstimatedThreshold :: B.C f Seconds,
    includeDriverCurrentlyOnRide :: B.C f Bool,
    defaultPopupDelay :: B.C f Seconds,
    popupDelayToAddAsPenalty :: B.C f (Maybe Seconds),
    thresholdCancellationScore :: B.C f (Maybe Int),
    minRidesForCancellationScore :: B.C f (Maybe Int),
    thresholdCancellationPercentageToUnlist :: B.C f (Maybe Int),
    minRidesToUnlist :: B.C f (Maybe Int),
    mediaFileUrlPattern :: B.C f Text,
    mediaFileSizeUpperLimit :: B.C f Int,
    referralLinkPassword :: B.C f Text,
    fcmUrl :: B.C f Text,
    fcmServiceAccount :: B.C f Text,
    fcmTokenKeyPrefix :: B.C f Text,
    onboardingTryLimit :: B.C f Int,
    onboardingRetryTimeInHours :: B.C f Int,
    checkImageExtractionForDashboard :: B.C f Bool,
    searchRepeatLimit :: B.C f Int,
    actualRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    upwardsRecomputeBuffer :: B.C f HighPrecMeters,
    approxRideDistanceDiffThreshold :: B.C f HighPrecMeters,
    driverPaymentCycleBuffer :: B.C f Seconds,
    driverPaymentCycleDuration :: B.C f Seconds,
    driverPaymentCycleStartTime :: B.C f Seconds,
    driverPaymentReminderInterval :: B.C f Seconds,
    driverAutoPayNotificationTime :: B.C f Seconds,
    driverAutoPayExecutionTime :: B.C f Seconds,
    rcLimit :: B.C f Int,
    automaticRCActivationCutOff :: B.C f Seconds,
    timeDiffFromUtc :: B.C f Seconds,
    subscription :: B.C f Bool,
    minLocationAccuracy :: B.C f Double,
    aadhaarVerificationRequired :: B.C f Bool,
    enableDashboardSms :: B.C f Bool,
    subscriptionStartTime :: B.C f UTCTime,
    mandateValidity :: B.C f Int,
    driverLocationAccuracyBuffer :: B.C f Meters,
    routeDeviationThreshold :: B.C f Meters,
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    registryUrl :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantConfigT where
  data PrimaryKey MerchantConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type MerchantConfig = MerchantConfigT Identity

merchantConfigTMod :: MerchantConfigT (B.FieldModification (B.TableField MerchantConfigT))
merchantConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      gstin = B.fieldNamed "gstin",
      name = B.fieldNamed "name",
      verified = B.fieldNamed "verified",
      enabled = B.fieldNamed "enabled",
      description = B.fieldNamed "description",
      mobileNumber = B.fieldNamed "mobile_number",
      mobileCountryCode = B.fieldNamed "mobile_country_code",
      fromTime = B.fieldNamed "from_time",
      toTime = B.fieldNamed "to_time",
      internalApiKey = B.fieldNamed "internal_api_key",
      headCount = B.fieldNamed "head_count",
      status = B.fieldNamed "status",
      info = B.fieldNamed "info",
      originRestriction = B.fieldNamed "origin_restriction",
      destinationRestriction = B.fieldNamed "destination_restriction",
      city = B.fieldNamed "city",
      country = B.fieldNamed "country",
      geoHashPrecisionValue = B.fieldNamed "geo_hash_precision_value",
      pickupLocThreshold = B.fieldNamed "pickup_loc_threshold",
      dropLocThreshold = B.fieldNamed "drop_loc_threshold",
      rideTimeEstimatedThreshold = B.fieldNamed "ride_time_estimated_threshold",
      includeDriverCurrentlyOnRide = B.fieldNamed "include_driver_currently_on_ride",
      defaultPopupDelay = B.fieldNamed "default_popup_delay",
      popupDelayToAddAsPenalty = B.fieldNamed "popup_delay_to_add_as_penalty",
      thresholdCancellationScore = B.fieldNamed "threshold_cancellation_score",
      minRidesForCancellationScore = B.fieldNamed "min_rides_for_cancellation_score",
      thresholdCancellationPercentageToUnlist = B.fieldNamed "threshold_cancellation_percentage_to_unlist",
      minRidesToUnlist = B.fieldNamed "min_rides_to_unlist",
      mediaFileUrlPattern = B.fieldNamed "media_file_url_pattern",
      mediaFileSizeUpperLimit = B.fieldNamed "media_file_size_upper_limit",
      referralLinkPassword = B.fieldNamed "referral_link_password",
      fcmUrl = B.fieldNamed "fcm_url",
      fcmServiceAccount = B.fieldNamed "fcm_service_account",
      fcmTokenKeyPrefix = B.fieldNamed "fcm_token_key_prefix",
      onboardingTryLimit = B.fieldNamed "onboarding_try_limit",
      onboardingRetryTimeInHours = B.fieldNamed "onboarding_retry_time_in_hours",
      checkImageExtractionForDashboard = B.fieldNamed "check_image_extraction_for_dashboard",
      searchRepeatLimit = B.fieldNamed "search_repeat_limit",
      actualRideDistanceDiffThreshold = B.fieldNamed "actual_ride_distance_diff_threshold",
      upwardsRecomputeBuffer = B.fieldNamed "upwards_recompute_buffer",
      approxRideDistanceDiffThreshold = B.fieldNamed "approx_ride_distance_diff_threshold",
      minimumDriverRatesCount = B.fieldNamed "minimum_driver_rates_count",
      driverPaymentCycleBuffer = B.fieldNamed "driver_payment_cycle_buffer",
      driverPaymentCycleDuration = B.fieldNamed "driver_payment_cycle_duration",
      driverPaymentCycleStartTime = B.fieldNamed "driver_payment_cycle_start_time",
      driverPaymentReminderInterval = B.fieldNamed "driver_payment_reminder_interval",
      timeDiffFromUtc = B.fieldNamed "time_diff_from_utc",
      subscription = B.fieldNamed "subscription",
      minLocationAccuracy = B.fieldNamed "min_location_accuracy",
      aadhaarVerificationRequired = B.fieldNamed "aadhaar_verification_required",
      enableDashboardSms = B.fieldNamed "enable_dashboard_sms",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      rcLimit = B.fieldNamed "rc_limit",
      mandateValidity = B.fieldNamed "mandate_validity",
      automaticRCActivationCutOff = B.fieldNamed "automatic_r_c_activation_cut_off",
      driverAutoPayNotificationTime = B.fieldNamed "driver_auto_pay_notification_time",
      driverAutoPayExecutionTime = B.fieldNamed "driver_auto_pay_execution_time",
      subscriptionStartTime = B.fieldNamed "subscription_start_time",
      driverLocationAccuracyBuffer = B.fieldNamed "driver_location_accuracy_buffer",
      routeDeviationThreshold = B.fieldNamed "route_deviation_threshold",
      canDowngradeToSedan = B.fieldNamed "can_downgrade_to_sedan",
      canDowngradeToHatchback = B.fieldNamed "can_downgrade_to_hatchback",
      registryUrl = B.fieldNamed "registry_url",
      canDowngradeToTaxi = B.fieldNamed "can_downgrade_to_taxi"
    }

$(enableKVPG ''MerchantConfigT ['merchantId] [])

$(mkTableInstances ''MerchantConfigT "merchant_config" "atlas_driver_offer_bpp")
