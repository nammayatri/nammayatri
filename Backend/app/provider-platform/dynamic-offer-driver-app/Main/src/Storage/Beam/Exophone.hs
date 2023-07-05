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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Storage.Beam.Exophone where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Database.Beam.Schema.Tables as B
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.UtilsTH
import Sequelize

data ExophoneT f = ExophoneT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    primaryPhone :: B.C f Text,
    backupPhone :: B.C f Text,
    isPrimaryDown :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

dExophone :: B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ExophoneT)
dExophone =
  B.setEntitySchema (Just "atlas_driver_offer_bpp")
    <> B.setEntityName "exophone"
    <> B.modifyTableFields exophoneTMod

instance B.Table ExophoneT where
  data PrimaryKey ExophoneT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta ExophoneT where
  modelFieldModification = exophoneTMod
  modelTableName = "exophone"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Exophone = ExophoneT Identity

instance FromJSON Exophone where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Exophone where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Exophone

exophoneTMod :: ExophoneT (B.FieldModification (B.TableField ExophoneT))
exophoneTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      primaryPhone = B.fieldNamed "primary_phone",
      backupPhone = B.fieldNamed "backup_phone",
      isPrimaryDown = B.fieldNamed "is_primary_down",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

exophoneToHSModifiers :: M.Map Text (A.Value -> A.Value)
exophoneToHSModifiers =
  M.empty

exophoneToPSModifiers :: M.Map Text (A.Value -> A.Value)
exophoneToPSModifiers =
  M.empty

instance Serialize Exophone where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''ExophoneT ['id] [['merchantId], ['primaryPhone]])
