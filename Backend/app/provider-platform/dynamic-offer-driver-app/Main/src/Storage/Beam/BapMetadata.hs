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

module Storage.Beam.BapMetadata where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data BapMetadataT f = BapMetadataT
  { id :: B.C f Text,
    name :: B.C f Text,
    logoUrl :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table BapMetadataT where
  data PrimaryKey BapMetadataT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta BapMetadataT where
  modelFieldModification = bapMetadataTMod
  modelTableName = "bap_metadata"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type BapMetadata = BapMetadataT Identity

instance FromJSON BapMetadata where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON BapMetadata where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show BapMetadata

bapMetadataTMod :: BapMetadataT (B.FieldModification (B.TableField BapMetadataT))
bapMetadataTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      name = B.fieldNamed "name",
      logoUrl = B.fieldNamed "logo_url"
    }

defaultBapMetadata :: BapMetadata
defaultBapMetadata =
  BapMetadataT
    { id = "",
      name = "",
      logoUrl = ""
    }

instance Serialize BapMetadata where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

bapMetadataToHSModifiers :: M.Map Text (A.Value -> A.Value)
bapMetadataToHSModifiers =
  M.empty

bapMetadataToPSModifiers :: M.Map Text (A.Value -> A.Value)
bapMetadataToPSModifiers =
  M.empty

$(enableKVPG ''BapMetadataT ['id] [])
