{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.Issue.IssueReport where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Issue.IssueReport as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Sequelize
import Tools.Beam.UtilsTH

data IssueReportT f = IssueReportT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    description :: B.C f Text,
    assignee :: B.C f (Maybe Text),
    status :: B.C f Domain.IssueStatus,
    categoryId :: B.C f Text,
    optionId :: B.C f (Maybe Text),
    deleted :: B.C f Bool,
    mediaFiles :: B.C f [Text],
    ticketId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueReportT where
  data PrimaryKey IssueReportT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueReport = IssueReportT Identity

$(enableKVPG ''IssueReportT ['id] [['driverId], ['categoryId], ['ticketId]])

$(mkTableInstances ''IssueReportT "issue_report")
