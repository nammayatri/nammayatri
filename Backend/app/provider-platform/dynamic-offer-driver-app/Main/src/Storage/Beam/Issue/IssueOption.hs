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

module Storage.Beam.Issue.IssueOption where

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

data IssueOptionT f = IssueOptionT
  { id :: B.C f Text,
    issueCategoryId :: B.C f Text,
    option :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueOptionT where
  data PrimaryKey IssueOptionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta IssueOptionT where
  modelFieldModification = issueOptionTMod
  modelTableName = "issue_option"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type IssueOption = IssueOptionT Identity

instance FromJSON IssueOption where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON IssueOption where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show IssueOption

issueOptionTMod :: IssueOptionT (B.FieldModification (B.TableField IssueOptionT))
issueOptionTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      issueCategoryId = B.fieldNamed "issue_category_id",
      option = B.fieldNamed "option"
    }

instance Serialize IssueOption where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

issueOptionToHSModifiers :: M.Map Text (A.Value -> A.Value)
issueOptionToHSModifiers =
  M.empty

issueOptionToPSModifiers :: M.Map Text (A.Value -> A.Value)
issueOptionToPSModifiers =
  M.empty

$(enableKVPG ''IssueOptionT ['id] [['issueCategoryId]])
