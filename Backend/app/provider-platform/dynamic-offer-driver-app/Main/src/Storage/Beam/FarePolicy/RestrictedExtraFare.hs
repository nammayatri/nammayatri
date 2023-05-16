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

module Storage.Beam.FarePolicy.RestrictedExtraFare where

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
import qualified Domain.Types.FarePolicy.RestrictedExtraFare as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Meters, Money)
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

instance FromField Meters where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Meters where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Meters

instance FromBackendRow Postgres Meters

instance FromField Vehicle.Variant where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Vehicle.Variant where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Vehicle.Variant

instance FromBackendRow Postgres Vehicle.Variant

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

data RestrictedExtraFareT f = RestrictedExtraFareT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    vehicleVariant :: B.C f Vehicle.Variant,
    minTripDistance :: B.C f Meters,
    driverMaxExtraFare :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table RestrictedExtraFareT where
  data PrimaryKey RestrictedExtraFareT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta RestrictedExtraFareT where
  modelFieldModification = restrictedExtraFareTMod
  modelTableName = "restricted_extra_fare"
  mkExprWithDefault _ = B.insertExpressions []

type RestrictedExtraFare = RestrictedExtraFareT Identity

instance FromJSON RestrictedExtraFare where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON RestrictedExtraFare where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show RestrictedExtraFare

deriving stock instance Read Money

restrictedExtraFareTMod :: RestrictedExtraFareT (B.FieldModification (B.TableField RestrictedExtraFareT))
restrictedExtraFareTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      vehicleVariant = B.fieldNamed "vehicle_variant",
      minTripDistance = B.fieldNamed "min_trip_distance",
      driverMaxExtraFare = B.fieldNamed "driver_max_extra_fare"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

restrictedExtraFareToHSModifiers :: M.Map Text (A.Value -> A.Value)
restrictedExtraFareToHSModifiers =
  M.fromList
    []

restrictedExtraFareToPSModifiers :: M.Map Text (A.Value -> A.Value)
restrictedExtraFareToPSModifiers =
  M.fromList
    []

instance IsString Vehicle.Variant where
  fromString = show

instance IsString Meters where
  fromString = show

instance IsString Money where
  fromString = show

defaultRestrictedExtraFare :: RestrictedExtraFare
defaultRestrictedExtraFare =
  RestrictedExtraFareT
    { id = "",
      merchantId = "",
      vehicleVariant = "",
      minTripDistance = "",
      driverMaxExtraFare = ""
    }

instance Serialize RestrictedExtraFare where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''RestrictedExtraFareT ['id] [])
