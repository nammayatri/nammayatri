{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Issue.Comment where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data CommentT f = CommentT
  { id :: B.C f Text,
    issueReportId :: B.C f Text,
    authorId :: B.C f Text,
    comment :: B.C f Text,
    createdAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CommentT where
  data PrimaryKey CommentT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Comment = CommentT Identity

commentTMod :: CommentT (B.FieldModification (B.TableField CommentT))
commentTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      issueReportId = B.fieldNamed "issue_report_id",
      authorId = B.fieldNamed "author_id",
      comment = B.fieldNamed "comment",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''CommentT ['id] [['issueReportId]])

$(mkTableInstances ''CommentT "comment" "atlas_driver_offer_bpp")
