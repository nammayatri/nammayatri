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

module Storage.Beam.Issue where

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

data IssueT f = IssueT
  { id :: B.C f Text,
    customerId :: B.C f Text,
    bookingId :: B.C f (Maybe Text),
    contactEmail :: B.C f (Maybe Text),
    reason :: B.C f Text,
    description :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueT where
  data PrimaryKey IssueT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta IssueT where
  modelFieldModification = issueTMod
  modelTableName = "issue"
  modelSchemaName = Just "atlas_app"

type Issue = IssueT Identity

instance FromJSON Issue where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Issue where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Issue

issueTMod :: IssueT (B.FieldModification (B.TableField IssueT))
issueTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      customerId = B.fieldNamed "customer_id",
      bookingId = B.fieldNamed "booking_id",
      contactEmail = B.fieldNamed "contact_email",
      reason = B.fieldNamed "reason",
      description = B.fieldNamed "description",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

defaultIssue :: Issue
defaultIssue =
  IssueT
    { id = "",
      customerId = "",
      bookingId = Nothing,
      contactEmail = Nothing,
      reason = "",
      description = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate
    }

instance Serialize Issue where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

issueToHSModifiers :: M.Map Text (A.Value -> A.Value)
issueToHSModifiers =
  M.empty

issueToPSModifiers :: M.Map Text (A.Value -> A.Value)
issueToPSModifiers =
  M.empty

$(enableKVPG ''IssueT ['id] [])
