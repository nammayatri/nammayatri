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
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Coerce (coerce)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Domain.Types.Common
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.Vehicle as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common (encodeToText)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

instance FromBackendRow Postgres [Domain.EstimateBreakup]

instance FromField [Domain.EstimateBreakup] where
  fromField f mbValue = V.toList <$> fromField f mbValue

instance FromField Domain.EstimateBreakup where
  fromField = fromFieldJSON

fromFieldEstimateBreakUp ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion [Domain.EstimateBreakup]
fromFieldEstimateBreakUp f mbValue = case mbValue of
  Nothing -> DPSF.returnError DPSF.UnexpectedNull f mempty
  Just value' -> case A.decode $ fromStrict value' of
    Just res -> pure res
    Nothing -> DPSF.returnError DPSF.ConversionFailed f "Could not 'read' value for 'Rule'."

instance (HasSqlValueSyntax be (V.Vector Text)) => HasSqlValueSyntax be [Domain.EstimateBreakup] where
  sqlValueSyntax estimateBreakupList =
    let unsafeEstimateBreakupList = coerce @[Domain.EstimateBreakup] @[Domain.EstimateBreakupD 'Unsafe] $ estimateBreakupList
        x = encodeToText <$> unsafeEstimateBreakupList
     in sqlValueSyntax (V.fromList x)

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Domain.EstimateBreakup]

data EstimateT f = EstimateT
  { id :: B.C f Text,
    requestId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    minFare :: B.C f Money,
    maxFare :: B.C f Money,
    estimateBreakupList :: B.C f [Domain.EstimateBreakup],
    nightShiftCharge :: B.C f (Maybe Money),
    oldNightShiftCharge :: B.C f (Maybe Centesimal),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    waitingChargePerMin :: B.C f (Maybe Money),
    waitingOrPickupCharges :: B.C f (Maybe Money),
    specialLocationTag :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Domain.EstimateBreakup where
  fromString = show

-- instance IsString Money where
--   fromString = show

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
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Estimate = EstimateT Identity

instance FromJSON Estimate where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Estimate where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Estimate

deriving stock instance Read Estimate

estimateTMod :: EstimateT (B.FieldModification (B.TableField EstimateT))
estimateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      requestId = B.fieldNamed "request_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      minFare = B.fieldNamed "min_fare",
      maxFare = B.fieldNamed "max_fare",
      estimateBreakupList = B.fieldNamed "estimate_breakup_list",
      nightShiftCharge = B.fieldNamed "night_shift_charge",
      oldNightShiftCharge = B.fieldNamed "night_shift_multiplier",
      nightShiftStart = B.fieldNamed "night_shift_start",
      nightShiftEnd = B.fieldNamed "night_shift_end",
      waitingChargePerMin = B.fieldNamed "waiting_charge_per_min",
      waitingOrPickupCharges = B.fieldNamed "waiting_or_pickup_charges",
      specialLocationTag = B.fieldNamed "special_location_tag",
      createdAt = B.fieldNamed "created_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

estimateToHSModifiers :: M.Map Text (A.Value -> A.Value)
estimateToHSModifiers =
  M.empty

estimateToPSModifiers :: M.Map Text (A.Value -> A.Value)
estimateToPSModifiers =
  M.empty

instance Serialize Estimate where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''EstimateT ['id] [])
