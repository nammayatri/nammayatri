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
import qualified Domain.Types.Issue.IssueOption as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Issue.IssueCategory (IssueCategoryTId)

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
  mkExprWithDefault _ = B.insertExpressions []

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

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

issueOptionToHSModifiers :: M.Map Text (A.Value -> A.Value)
issueOptionToHSModifiers =
  M.fromList
    []

issueOptionToPSModifiers :: M.Map Text (A.Value -> A.Value)
issueOptionToPSModifiers =
  M.fromList
    []

instance Serialize IssueOption where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''IssueOptionT ['id] [])
