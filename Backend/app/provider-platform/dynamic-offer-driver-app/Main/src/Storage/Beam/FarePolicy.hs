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

module Storage.Beam.FarePolicy where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Vehicle ()

instance FromField Domain.FarePolicyType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.FarePolicyType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FarePolicyType

instance FromBackendRow Postgres Domain.FarePolicyType

data FarePolicyT f = FarePolicyT
  { id :: B.C f Text,
    farePolicyType :: B.C f Domain.FarePolicyType,
    serviceCharge :: B.C f (Maybe Money),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    maxAllowedTripDistance :: B.C f (Maybe Meters),
    minAllowedTripDistance :: B.C f (Maybe Meters),
    govtCharges :: B.C f (Maybe Double),
    description :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance IsString Variant.Variant where
  fromString = show

instance IsString Domain.FarePolicyType where
  fromString = show

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FarePolicyT where
  modelFieldModification = farePolicyTMod
  modelTableName = "fare_policy"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type FarePolicy = FarePolicyT Identity

instance FromJSON FarePolicy where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FarePolicy where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.FarePolicyType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.FarePolicyType where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FarePolicy

deriving stock instance Ord Domain.FarePolicyType

deriving stock instance Eq Domain.FarePolicyType

-- deriving stock instance Read Money

farePolicyTMod :: FarePolicyT (B.FieldModification (B.TableField FarePolicyT))
farePolicyTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      farePolicyType = B.fieldNamed "fare_policy_type",
      serviceCharge = B.fieldNamed "service_charge",
      nightShiftStart = B.fieldNamed "night_shift_start",
      nightShiftEnd = B.fieldNamed "night_shift_end",
      maxAllowedTripDistance = B.fieldNamed "max_allowed_trip_distance",
      minAllowedTripDistance = B.fieldNamed "min_allowed_trip_distance",
      govtCharges = B.fieldNamed "govt_charges",
      description = B.fieldNamed "description",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

farePolicyToHSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicyToHSModifiers =
  M.empty

farePolicyToPSModifiers :: M.Map Text (A.Value -> A.Value)
farePolicyToPSModifiers =
  M.empty

defaultFarePolicy :: FarePolicy
defaultFarePolicy =
  FarePolicyT
    { id = "",
      farePolicyType = "",
      serviceCharge = Nothing,
      nightShiftStart = Nothing,
      nightShiftEnd = Nothing,
      maxAllowedTripDistance = Nothing,
      minAllowedTripDistance = Nothing,
      govtCharges = Nothing,
      description = Nothing,
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize FarePolicy where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''FarePolicyT ['id] [[]])
