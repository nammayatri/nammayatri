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

module Storage.Beam.Issue.Comment where

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

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

data CommentT f = CommentT
  { id :: B.C f Text,
    issueReportId :: B.C f Text,
    authorId :: B.C f Text,
    comment :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CommentT where
  data PrimaryKey CommentT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta CommentT where
  modelFieldModification = commentTMod
  modelTableName = "comment"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Comment = CommentT Identity

instance FromJSON Comment where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Comment where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Comment

commentTMod :: CommentT (B.FieldModification (B.TableField CommentT))
commentTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      issueReportId = B.fieldNamed "issue_report_id",
      authorId = B.fieldNamed "author_id",
      comment = B.fieldNamed "comment",
      createdAt = B.fieldNamed "created_at"
    }

defaultComment :: Comment
defaultComment =
  CommentT
    { id = "",
      issueReportId = "",
      authorId = "",
      comment = "",
      createdAt = defaultUTCDate
    }

instance Serialize Comment where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

commentToHSModifiers :: M.Map Text (A.Value -> A.Value)
commentToHSModifiers =
  M.empty

commentToPSModifiers :: M.Map Text (A.Value -> A.Value)
commentToPSModifiers =
  M.empty

$(enableKVPG ''CommentT ['id] [])
