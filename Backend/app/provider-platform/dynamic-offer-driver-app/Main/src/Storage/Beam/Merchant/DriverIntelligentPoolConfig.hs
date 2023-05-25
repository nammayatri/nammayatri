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

module Storage.Beam.Merchant.DriverIntelligentPoolConfig where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common
import Kernel.Types.Common hiding (id)
import Kernel.Types.SlidingWindowCounters (PeriodType)
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)

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

instance FromField Minutes where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Minutes where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Minutes

instance FromBackendRow Postgres Minutes

instance IsString Minutes where
  fromString = show

instance FromField SWC.SlidingWindowOptions where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SWC.SlidingWindowOptions where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be SWC.SlidingWindowOptions

instance FromBackendRow Postgres SWC.SlidingWindowOptions

instance IsString SWC.SlidingWindowOptions where
  fromString = show

data DriverIntelligentPoolConfigT f = DriverIntelligentPoolConfigT
  { merchantId :: B.C f Text,
    availabilityTimeWeightage :: B.C f Int,
    availabilityTimeWindowOption :: B.C f SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: B.C f Int,
    acceptanceRatioWindowOption :: B.C f SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: B.C f Int,
    cancellationRatioWindowOption :: B.C f SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: B.C f Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: B.C f SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: B.C f (Maybe Int),
    speedNormalizer :: B.C f Double,
    driverSpeedWeightage :: B.C f Int,
    locationUpdateSampleTime :: B.C f Minutes,
    minLocationUpdates :: B.C f Int,
    defaultDriverSpeed :: B.C f Double,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverIntelligentPoolConfigT where
  data PrimaryKey DriverIntelligentPoolConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantId

instance ModelMeta DriverIntelligentPoolConfigT where
  modelFieldModification = driverIntelligentPoolConfigTMod
  modelTableName = "driver_intelligent_pool_config"
  mkExprWithDefault _ = B.insertExpressions []

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigT Identity

instance FromJSON DriverIntelligentPoolConfig where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverIntelligentPoolConfig where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverIntelligentPoolConfig

deriving stock instance Ord SWC.SlidingWindowOptions

deriving stock instance Eq SWC.SlidingWindowOptions

deriving stock instance Ord PeriodType

driverIntelligentPoolConfigTMod :: DriverIntelligentPoolConfigT (B.FieldModification (B.TableField DriverIntelligentPoolConfigT))
driverIntelligentPoolConfigTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      availabilityTimeWeightage = B.fieldNamed "availability_time_weightage",
      availabilityTimeWindowOption = B.fieldNamed "availability_time_window_option",
      acceptanceRatioWeightage = B.fieldNamed "acceptance_ratio_weightage",
      acceptanceRatioWindowOption = B.fieldNamed "acceptance_ratio_window_option",
      cancellationRatioWeightage = B.fieldNamed "cancellation_ratio_weightage",
      cancellationRatioWindowOption = B.fieldNamed "cancellation_ratio_window_option",
      minQuotesToQualifyForIntelligentPool = B.fieldNamed "min_quotes_to_qualify_for_intelligent_pool",
      minQuotesToQualifyForIntelligentPoolWindowOption = B.fieldNamed "min_quotes_to_qualify_for_intelligent_pool_window_option",
      intelligentPoolPercentage = B.fieldNamed "intelligent_pool_percentage",
      speedNormalizer = B.fieldNamed "speed_normalizer",
      driverSpeedWeightage = B.fieldNamed "driver_speed_weightage",
      locationUpdateSampleTime = B.fieldNamed "location_update_sample_time",
      minLocationUpdates = B.fieldNamed "min_location_updates",
      defaultDriverSpeed = B.fieldNamed "default_driver_speed",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

-- instance IsString SWC.SlidingWindowOptions where
--   fromString = show

-- instance IsString Minutes where
-- fromString = show

-- defaultDriverIntelligentPoolConfig :: DriverIntelligentPoolConfig
-- defaultDriverIntelligentPoolConfig =
--   DriverIntelligentPoolConfigT
--     { merchantId = "",
--       availabilityTimeWeightage = 0,
--       availabilityTimeWindowOption = "",
--       acceptanceRatioWeightage = 0,
--       acceptanceRatioWindowOption = "",
--       cancellationRatioWeightage = 0,
--       cancellationRatioWindowOption = "",
--       minQuotesToQualifyForIntelligentPool = 0,
--       minQuotesToQualifyForIntelligentPoolWindowOption = "",
--       intelligentPoolPercentage = Nothing,
--       speedNormalizer = 0.0,
--       driverSpeedWeightage = 0,
--       locationUpdateSampleTime = "",
--       minLocationUpdates = 0,
--       defaultDriverSpeed = 0.0,
--       createdAt = defaultUTCDate,
--       updatedAt = defaultUTCDate
--     }

instance Serialize DriverIntelligentPoolConfig where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverIntelligentPoolConfigToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverIntelligentPoolConfigToHSModifiers =
  M.fromList
    []

driverIntelligentPoolConfigToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverIntelligentPoolConfigToPSModifiers =
  M.fromList
    []

$(enableKVPG ''DriverIntelligentPoolConfigT ['merchantId] [])
