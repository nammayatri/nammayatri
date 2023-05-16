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

module Storage.Beam.Estimate where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import Data.Coerce (coerce)
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
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.Vehicle as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()
import Tools.Error

fromFieldEnum ::
  (Typeable a, Read a) =>
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion a
fromFieldEnum f mbValue = case mbValue of
  Nothing -> DPSF.returnError UnexpectedNull f mempty
  Just value' ->
    case (readMaybe (unpackChars value')) of
      Just val -> pure val
      _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

instance FromField Variant.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Variant.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Variant.Variant

instance FromBackendRow Postgres Variant.Variant

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance FromBackendRow Postgres Centesimal

-- instance FromField TimeOfDay where
--   fromField = fromFieldEnum

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be TimeOfDay where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be TimeOfDay

-- instance FromBackendRow Postgres TimeOfDay

instance FromField Seconds where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Seconds where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Seconds

instance FromBackendRow Postgres Seconds

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Domain.EstimateBreakup] where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Domain.EstimateBreakup]

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

data EstimateT f = EstimateT
  { id :: B.C f Text,
    transactionId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    minFare :: B.C f Money,
    maxFare :: B.C f Money,
    estimateBreakupList :: B.C f [Domain.EstimateBreakup],
    nightShiftMultiplier :: B.C f (Maybe Centesimal),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    waitingTimeEstimatedThreshold :: B.C f (Maybe Seconds),
    waitingChargePerMin :: B.C f (Maybe Money),
    waitingOrPickupCharges :: B.C f (Maybe Money),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.EstimateBreakup where
  fromString = show

instance IsString Money where
  fromString = show

instance IsString Variant.Variant where
  fromString = show

instance B.Table EstimateT where
  data PrimaryKey EstimateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta EstimateT where
  modelFieldModification = estimateTMod
  modelTableName = "estimate"
  mkExprWithDefault _ = B.insertExpressions []

type Estimate = EstimateT Identity

instance FromJSON Estimate where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Estimate where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Estimate

deriving stock instance Read Money

estimateTMod :: EstimateT (B.FieldModification (B.TableField EstimateT))
estimateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      transactionId = B.fieldNamed "transaction_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      minFare = B.fieldNamed "min_fare",
      maxFare = B.fieldNamed "max_fare",
      estimateBreakupList = B.fieldNamed "estimate_breakup_list",
      nightShiftMultiplier = B.fieldNamed "night_shift_multiplier",
      nightShiftStart = B.fieldNamed "night_shift_start",
      nightShiftEnd = B.fieldNamed "night_shift_end",
      waitingTimeEstimatedThreshold = B.fieldNamed "waiting_time_estimated_threshold",
      waitingChargePerMin = B.fieldNamed "waiting_charge_per_min",
      waitingOrPickupCharges = B.fieldNamed "waiting_or_pickup_charges",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

estimateToHSModifiers :: M.Map Text (A.Value -> A.Value)
estimateToHSModifiers =
  M.fromList
    []

estimateToPSModifiers :: M.Map Text (A.Value -> A.Value)
estimateToPSModifiers =
  M.fromList
    []

defaultEstimate :: Estimate
defaultEstimate =
  EstimateT
    { id = "",
      transactionId = "",
      vehicleVariant = "",
      minFare = "",
      maxFare = "",
      estimateBreakupList = [""],
      nightShiftMultiplier = Nothing,
      nightShiftStart = Nothing,
      nightShiftEnd = Nothing,
      waitingTimeEstimatedThreshold = Nothing,
      waitingChargePerMin = Nothing,
      waitingOrPickupCharges = Nothing,
      createdAt = defaultUTCDate
    }

instance Serialize Estimate where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''EstimateT ['id] [])
