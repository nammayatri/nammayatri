{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.MetaData where

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
import Lib.UtilsTH
import Sequelize

data MetaDataT f = MetaDataT
  { driverId :: B.C f Text,
    device :: B.C f (Maybe Text),
    deviceOS :: B.C f (Maybe Text),
    deviceDateTime :: B.C f (Maybe Time.UTCTime),
    appPermissions :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MetaDataT where
  data PrimaryKey MetaDataT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

instance ModelMeta MetaDataT where
  modelFieldModification = metaDataTMod
  modelTableName = "meta_data"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MetaData = MetaDataT Identity

instance FromJSON MetaData where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MetaData where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MetaData

metaDataTMod :: MetaDataT (B.FieldModification (B.TableField MetaDataT))
metaDataTMod =
  B.tableModification
    { driverId = B.fieldNamed "driver_id",
      device = B.fieldNamed "device",
      deviceOS = B.fieldNamed "device_o_s",
      deviceDateTime = B.fieldNamed "device_date_time",
      appPermissions = B.fieldNamed "app_permissions",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

-- defaultMetaData :: MetaData
-- defaultMetaData =
--   MetaDataT
--     {
--       driverId = "",
--       device = Nothing,
--       deviceOS = Nothing,
--       deviceDateTime = Nothing,
--       appPermissions = Nothing,
--       createdAt = defaultUTCDate,
--       updatedAt = defaultUTCDate
--     }

instance Serialize MetaData where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

metaDataToHSModifiers :: M.Map Text (A.Value -> A.Value)
metaDataToHSModifiers =
  M.empty

metaDataToPSModifiers :: M.Map Text (A.Value -> A.Value)
metaDataToPSModifiers =
  M.empty

$(enableKVPG ''MetaDataT ['driverId] [])
