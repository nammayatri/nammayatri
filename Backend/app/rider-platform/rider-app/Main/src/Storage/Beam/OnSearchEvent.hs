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

module Storage.Beam.OnSearchEvent where

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
import Lib.Utils
import Lib.UtilsTH
import Sequelize

data OnSearchEventT f = OnSearchEventT
  { id :: B.C f Text,
    bppId :: B.C f Text,
    messageId :: B.C f Text,
    errorCode :: B.C f (Maybe Text),
    errorType :: B.C f (Maybe Text),
    errorMessage :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OnSearchEventT where
  data PrimaryKey OnSearchEventT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta OnSearchEventT where
  modelFieldModification = onSearchEventTMod
  modelTableName = "on_search_event"
  modelSchemaName = Just "atlas_app"

type OnSearchEvent = OnSearchEventT Identity

instance FromJSON OnSearchEvent where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON OnSearchEvent where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show OnSearchEvent

onSearchEventTMod :: OnSearchEventT (B.FieldModification (B.TableField OnSearchEventT))
onSearchEventTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      bppId = B.fieldNamed "bpp_id",
      messageId = B.fieldNamed "message_id",
      errorCode = B.fieldNamed "error_code",
      errorType = B.fieldNamed "error_type",
      errorMessage = B.fieldNamed "error_message",
      createdAt = B.fieldNamed "created_at"
    }

defaultOnSearchEvent :: OnSearchEvent
defaultOnSearchEvent =
  OnSearchEventT
    { id = "",
      bppId = "",
      messageId = "",
      errorCode = Nothing,
      errorType = Nothing,
      errorMessage = Nothing,
      createdAt = defaultUTCDate
    }

instance Serialize OnSearchEvent where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

onSearchEventToHSModifiers :: M.Map Text (A.Value -> A.Value)
onSearchEventToHSModifiers =
  M.empty

onSearchEventToPSModifiers :: M.Map Text (A.Value -> A.Value)
onSearchEventToPSModifiers =
  M.empty

$(enableKVPG ''OnSearchEventT ['id] [])
