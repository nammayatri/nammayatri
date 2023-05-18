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

module Storage.Beam.SlabFarePolicy where

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
import qualified Domain.Types.SlabFarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Centesimal, Meters, Money)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Vehicle ()

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

instance HasSqlValueSyntax be String => HasSqlValueSyntax be [Domain.Slab] where
  sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be TimeOfDay

-- instance FromBackendRow Postgres TimeOfDay

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance BeamSqlBackend be => B.HasSqlEqualityCheck be [Domain.Slab]

instance FromBackendRow Postgres Meters

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

data SlabFarePolicyT f = SlabFarePolicyT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    nightShiftRate :: B.C f (Maybe Centesimal),
    maxAllowedTripDistance :: B.C f (Maybe Meters),
    minAllowedTripDistance :: B.C f (Maybe Meters),
    serviceCharge :: B.C f Money,
    fareSlabs :: B.C f [Domain.Slab],
    govtChargesPerc :: B.C f (Maybe Int),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Variant.Variant where
  fromString = show

instance IsString Money where
  fromString = show

instance B.Table SlabFarePolicyT where
  data PrimaryKey SlabFarePolicyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta SlabFarePolicyT where
  modelFieldModification = slabFarePolicyTMod
  modelTableName = "slab_fare_policy"
  mkExprWithDefault _ = B.insertExpressions []

type SlabFarePolicy = SlabFarePolicyT Identity

instance FromJSON SlabFarePolicy where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON SlabFarePolicy where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show SlabFarePolicy

deriving stock instance Ord Domain.Slab

instance IsString Domain.Slab where
  fromString = show

slabFarePolicyTMod :: SlabFarePolicyT (B.FieldModification (B.TableField SlabFarePolicyT))
slabFarePolicyTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      nightShiftStart = B.fieldNamed "night_shift_start",
      nightShiftEnd = B.fieldNamed "night_shift_end",
      nightShiftRate = B.fieldNamed "night_shift_rate",
      maxAllowedTripDistance = B.fieldNamed "max_allowed_trip_distance",
      minAllowedTripDistance = B.fieldNamed "min_allowed_trip_distance",
      serviceCharge = B.fieldNamed "service_charge",
      fareSlabs = B.fieldNamed "fare_slabs",
      govtChargesPerc = B.fieldNamed "govt_charges_perc",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

slabFarePolicyToHSModifiers :: M.Map Text (A.Value -> A.Value)
slabFarePolicyToHSModifiers =
  M.fromList
    []

slabFarePolicyToPSModifiers :: M.Map Text (A.Value -> A.Value)
slabFarePolicyToPSModifiers =
  M.fromList
    []

defaultSlabFarePolicy :: SlabFarePolicy
defaultSlabFarePolicy =
  SlabFarePolicyT
    { id = "",
      merchantId = "",
      vehicleVariant = "",
      nightShiftStart = Nothing,
      nightShiftEnd = Nothing,
      nightShiftRate = Nothing,
      maxAllowedTripDistance = Nothing,
      minAllowedTripDistance = Nothing,
      serviceCharge = "",
      fareSlabs = [""],
      govtChargesPerc = Nothing,
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize SlabFarePolicy where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''SlabFarePolicyT ['id] [])
