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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Issue where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Issue as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (fromFieldEnum)
import Lib.Utils ()
import Sequelize

instance FromField Domain.IssueStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.IssueStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.IssueStatus

instance FromBackendRow Postgres Domain.IssueStatus

instance IsString Domain.IssueStatus where
  fromString = show

data IssueT f = IssueT
  { id :: B.C f Text,
    customerId :: B.C f Text,
    bookingId :: B.C f (Maybe Text),
    contactEmail :: B.C f (Maybe Text),
    reason :: B.C f Text,
    description :: B.C f Text,
    ticketId :: B.C f (Maybe Text),
    status :: B.C f Domain.IssueStatus,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueT where
  data PrimaryKey IssueT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Issue = IssueT Identity

issueTMod :: IssueT (B.FieldModification (B.TableField IssueT))
issueTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      customerId = B.fieldNamed "customer_id",
      bookingId = B.fieldNamed "booking_id",
      contactEmail = B.fieldNamed "contact_email",
      reason = B.fieldNamed "reason",
      description = B.fieldNamed "description",
      ticketId = B.fieldNamed "ticket_id",
      status = B.fieldNamed "status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''IssueT ['id] [['ticketId]])

$(mkTableInstances ''IssueT "issue" "atlas_app")
