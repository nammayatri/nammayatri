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
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.TransporterConfig where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

-- instance FromField Centesimal where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

-- instance FromBackendRow Postgres Centesimal

instance IsString HighPrecMeters where
  fromString = show

-- instance FromField Seconds where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

-- instance FromBackendRow Postgres Seconds

instance IsString Seconds where
  fromString = show

-- instance FromField Meters where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

-- instance FromBackendRow Postgres Meters

instance IsString Meters where
  fromString = show

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
    driverLeaderBoardExpiry :: B.C f (Maybe Seconds),
    minLocationAccuracy :: B.C f Double,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta TransporterConfigT where
  modelFieldModification = transporterConfigTMod
  modelTableName = "transporter_config"
  mkExprWithDefault _ = B.insertExpressions []

type TransporterConfig = TransporterConfigT Identity

instance FromJSON TransporterConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON TransporterConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show TransporterConfig

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
      driverLeaderBoardExpiry = B.fieldNamed "driver_leader_board_expiry",
      minLocationAccuracy = B.fieldNamed "min_location_accuracy",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

-- defaultTransporterConfig :: TransporterConfig
-- defaultTransporterConfig =
--   TransporterConfigT
--     { merchantId = "",
--       pickupLocThreshold = "",
--       dropLocThreshold = "",
--       rideTimeEstimatedThreshold = "",
--       includeDriverCurrentlyOnRide = False,
--       defaultPopupDelay = "",
--       popupDelayToAddAsPenalty = Nothing,
--       thresholdCancellationScore = Nothing,
--       minRidesForCancellationScore = Nothing,
--       mediaFileUrlPattern = "",
--       mediaFileSizeUpperLimit = 0,
--       waitingTimeEstimatedThreshold = "",
--       referralLinkPassword = "",
--       fcmUrl = "",
--       fcmServiceAccount = "",
--       fcmTokenKeyPrefix = "",
--       onboardingTryLimit = 0,
--       onboardingRetryTimeInHours = 0,
--       checkImageExtractionForDashboard = False,
--       searchRepeatLimit = 0,
--       actualRideDistanceDiffThreshold = "",
--       upwardsRecomputeBuffer = "",
--       approxRideDistanceDiffThreshold = "",
--       driverLeaderBoardExpiry = Nothing,
--       minLocationAccuracy = 0.0,
--       createdAt = defaultUTCDate,
--       updatedAt = defaultUTCDate
--     }

instance Serialize TransporterConfig where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

transporterConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
transporterConfigToHSModifiers =
  M.empty

transporterConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
transporterConfigToPSModifiers =
  M.empty

$(enableKVPG ''TransporterConfigT ['merchantId] [])
