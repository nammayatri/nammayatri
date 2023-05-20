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

module Storage.Beam.FareParameters where

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
import qualified Domain.Types.FareParameters as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (Centesimal, Money)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
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

instance FromField Centesimal where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Centesimal where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Centesimal

instance IsString Centesimal where
  fromString = show

instance FromBackendRow Postgres Centesimal

instance FromField Money where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.FareParametersType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.FareParametersType

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Money where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Money

instance FromBackendRow Postgres Money

-- data FareParametersT f = FareParametersT
--   { id :: B.C f Text,
--     baseFare :: B.C f Money,
--     deadKmFare :: B.C f (Maybe Money),
--     extraKmFare :: B.C f (Maybe Money),
--     driverSelectedFare :: B.C f (Maybe Money),
--     customerExtraFee :: B.C f (Maybe Money),
--     nightShiftRate :: B.C f (Maybe Centesimal),
--     nightCoefIncluded :: B.C f Bool,
--     waitingChargePerMin :: B.C f (Maybe Money),
--     waitingOrPickupCharges :: B.C f (Maybe Money),
--     serviceCharge :: B.C f (Maybe Money),
--     govtChargesPerc :: B.C f (Maybe Int)
--   }
--   deriving (Generic, B.Beamable)

data FareParametersT f = FareParametersT
  { id :: B.C f Text,
    baseFare :: B.C f Money,
    -- deadKmFare :: B.C f (Maybe Money),
    -- extraKmFare :: B.C f (Maybe Money),
    driverSelectedFare :: B.C f (Maybe Money),
    customerExtraFee :: B.C f (Maybe Money),
    -- nightShiftRate :: B.C f (Maybe Centesimal),
    -- nightCoefIncluded :: B.C f Bool,
    -- waitingChargePerMin :: B.C f (Maybe Money),
    waitingCharge :: B.C f (Maybe Money),
    nightShiftCharge :: B.C f (Maybe Money),
    -- waitingOrPickupCharges :: B.C f (Maybe Money),
    serviceCharge :: B.C f (Maybe Money),
    fareParametersType :: B.C f Domain.FareParametersType,
    govtCharges :: B.C f (Maybe Money)
  }
  deriving (Generic, B.Beamable)

instance IsString Money where
  fromString = show

instance B.Table FareParametersT where
  data PrimaryKey FareParametersT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta FareParametersT where
  modelFieldModification = fareParametersTMod
  modelTableName = "fare_parameters"
  mkExprWithDefault _ = B.insertExpressions []

type FareParameters = FareParametersT Identity

instance FromJSON FareParameters where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareParameters where
  toJSON = A.genericToJSON A.defaultOptions

instance FromJSON Domain.FareParametersType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Domain.FareParametersType where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show FareParameters

deriving stock instance Ord Domain.FareParametersType

deriving stock instance Eq Domain.FareParametersType

deriving stock instance Ord Domain.FarePolicyType

deriving stock instance Read Money

fareParametersTMod :: FareParametersT (B.FieldModification (B.TableField FareParametersT))
fareParametersTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      baseFare = B.fieldNamed "base_fare",
      -- deadKmFare = B.fieldNamed "dead_km_fare",
      -- extraKmFare = B.fieldNamed "extra_km_fare",
      driverSelectedFare = B.fieldNamed "driver_selected_fare",
      customerExtraFee = B.fieldNamed "customer_extra_fee",
      -- nightShiftRate = B.fieldNamed "night_shift_rate",
      -- nightCoefIncluded = B.fieldNamed "night_coef_included",
      -- waitingChargePerMin = B.fieldNamed "waiting_charge_per_min",
      -- waitingOrPickupCharges = B.fieldNamed "waiting_or_pickup_charges",
      serviceCharge = B.fieldNamed "service_charge",
      fareParametersType = B.fieldNamed "fare_parameters_type",
      govtCharges = B.fieldNamed "govt_charges"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

fareParametersToHSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersToHSModifiers =
  M.fromList
    []

fareParametersToPSModifiers :: M.Map Text (A.Value -> A.Value)
fareParametersToPSModifiers =
  M.fromList
    []

instance Serialize FareParameters where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''FareParametersT ['id] [])
