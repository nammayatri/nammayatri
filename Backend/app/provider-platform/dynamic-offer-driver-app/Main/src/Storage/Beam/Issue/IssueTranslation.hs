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

module Storage.Beam.Issue.IssueTranslation where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Types (Language)
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance IsString Language where
  fromString = show

data IssueTranslationT f = IssueTranslationT
  { id :: B.C f Text,
    sentence :: B.C f Text,
    translation :: B.C f Text,
    language :: B.C f Language
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueTranslationT where
  data PrimaryKey IssueTranslationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta IssueTranslationT where
  modelFieldModification = issueTranslationTMod
  modelTableName = "issue_translation"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type IssueTranslation = IssueTranslationT Identity

instance FromJSON IssueTranslation where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON IssueTranslation where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show IssueTranslation

issueTranslationTMod :: IssueTranslationT (B.FieldModification (B.TableField IssueTranslationT))
issueTranslationTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      sentence = B.fieldNamed "sentence",
      translation = B.fieldNamed "translation",
      language = B.fieldNamed "language"
    }

defaultIssueTranslation :: IssueTranslation
defaultIssueTranslation =
  IssueTranslationT
    { id = "",
      sentence = "",
      translation = "",
      language = ""
    }

instance Serialize IssueTranslation where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

issueTranslationToHSModifiers :: M.Map Text (A.Value -> A.Value)
issueTranslationToHSModifiers =
  M.empty

issueTranslationToPSModifiers :: M.Map Text (A.Value -> A.Value)
issueTranslationToPSModifiers =
  M.empty

$(enableKVPG ''IssueTranslationT ['id] [])
