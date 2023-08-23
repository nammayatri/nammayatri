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
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as BST
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data OperatingCityT f = OperatingCityT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    cityName :: B.C f Text,
    enabled :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
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
  modelSchemaName = Just "atlas_driver_offer_bpp"

type OperatingCity = OperatingCityT Identity

operatingCityTable :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity OperatingCityT)
operatingCityTable =
  BST.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "operating_city"
    <> B.modifyTableFields operatingCityTMod

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

instance Serialize OperatingCity where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

operatingCityToHSModifiers :: M.Map Text (A.Value -> A.Value)
operatingCityToHSModifiers =
  M.empty

operatingCityToPSModifiers :: M.Map Text (A.Value -> A.Value)
operatingCityToPSModifiers =
  M.empty

$(enableKVPG ''OperatingCityT ['id] [['merchantId], ['cityName]])
