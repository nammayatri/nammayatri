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

module Storage.Beam.DriverOnboarding.OperatingCity where

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
import qualified Domain.Types.DriverOnboarding.OperatingCity as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Merchant (MerchantTId)

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

data OperatingCityT f = OperatingCityT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    cityName :: B.C f Text,
    enabled :: B.C f Bool,
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OperatingCityT where
  data PrimaryKey OperatingCityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta OperatingCityT where
  modelFieldModification = operatingCityTMod
  modelTableName = "operating_city"
  mkExprWithDefault _ = B.insertExpressions []

type OperatingCity = OperatingCityT Identity

instance FromJSON OperatingCity where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON OperatingCity where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show OperatingCity

operatingCityTMod :: OperatingCityT (B.FieldModification (B.TableField OperatingCityT))
operatingCityTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      cityName = B.fieldNamed "city_name",
      enabled = B.fieldNamed "enabled",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

operatingCityToHSModifiers :: M.Map Text (A.Value -> A.Value)
operatingCityToHSModifiers =
  M.fromList
    []

operatingCityToPSModifiers :: M.Map Text (A.Value -> A.Value)
operatingCityToPSModifiers =
  M.fromList
    []

instance Serialize OperatingCity where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''OperatingCityT ['id] [])
