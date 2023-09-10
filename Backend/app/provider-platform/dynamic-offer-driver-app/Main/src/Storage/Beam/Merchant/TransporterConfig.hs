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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.TransporterConfig where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Lib.Utils ()
import Sequelize

data TransporterConfigT f = TransporterConfigT
  { merchantId :: B.C f Text,
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
    isAvoidToll :: B.C f Bool,
    timeDiffFromUtc :: B.C f Seconds,
    subscription :: B.C f Bool,
    minLocationAccuracy :: B.C f Double,
    aadhaarVerificationRequired :: B.C f Bool,
    enableDashboardSms :: B.C f Bool,
    subscriptionStartTime :: B.C f UTCTime,
    mandateValidity :: B.C f Int,
    bankErrorExpiry :: B.C f Seconds,
    driverLocationAccuracyBuffer :: B.C f Meters,
    routeDeviationThreshold :: B.C f Meters,
    canDowngradeToSedan :: B.C f Bool,
    canDowngradeToHatchback :: B.C f Bool,
    canDowngradeToTaxi :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

type TransporterConfig = TransporterConfigT Identity

transporterConfigTMod :: TransporterConfigT (B.FieldModification (B.TableField TransporterConfigT))
transporterConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
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
      canDowngradeToTaxi = B.fieldNamed "can_downgrade_to_taxi"
    }

$(enableKVPG ''TransporterConfigT ['merchantId] [])

$(mkTableInstances ''TransporterConfigT "transporter_config" "atlas_driver_offer_bpp")
