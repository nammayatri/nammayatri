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

module Storage.Beam.Message.MessageTranslation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import qualified Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

-- import Storage.Tabular.Person ()

instance IsString Language where
  fromString = show

data MessageTranslationT f = MessageTranslationT
  { messageId :: B.C f Text,
    language :: B.C f Language,
    title :: B.C f Text,
    description :: B.C f Text,
    shortDescription :: B.C f Text,
    label :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MessageTranslationT where
  data PrimaryKey MessageTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . messageId

instance ModelMeta MessageTranslationT where
  modelFieldModification = messageTranslationTMod
  modelTableName = "message_translation"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type MessageTranslation = MessageTranslationT Identity

instance FromJSON MessageTranslation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON MessageTranslation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show MessageTranslation

messageTranslationTMod :: MessageTranslationT (B.FieldModification (B.TableField MessageTranslationT))
messageTranslationTMod =
  B.tableModification
    { messageId = B.fieldNamed "message_id",
      language = B.fieldNamed "language",
      title = B.fieldNamed "title",
      description = B.fieldNamed "description",
      shortDescription = B.fieldNamed "short_description",
      label = B.fieldNamed "label",
      createdAt = B.fieldNamed "created_at"
    }

instance Data.Serialize.Serialize MessageTranslation where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

messageTranslationToHSModifiers :: M.Map Text (A.Value -> A.Value)
messageTranslationToHSModifiers =
  M.empty

messageTranslationToPSModifiers :: M.Map Text (A.Value -> A.Value)
messageTranslationToPSModifiers =
  M.empty

$(enableKVPG ''MessageTranslationT ['messageId] [])
