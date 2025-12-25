{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Storage.Beam.Issue.IssueChat where

import Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import IssueManagement.Tools.UtilsTH

data IssueChatT f = IssueChatT
  { id :: B.C f Text,
    ticketId :: B.C f Text,
    chats :: B.C f [Text],
    mediaFiles :: B.C f [Text],
    kaptureData :: B.C f (Maybe Text),
    personId :: B.C f Text,
    rideId :: B.C f (Maybe Text),
    issueReportId :: B.C f (Maybe Text),
    createdAt :: B.C f Time.LocalTime,
    updatedAt :: B.C f Time.LocalTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueChatT where
  data PrimaryKey IssueChatT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type IssueChat = IssueChatT Identity

$(enableKVPG ''IssueChatT ['id] [['personId], ['issueReportId]])

$(mkTableInstancesGenericSchema ''IssueChatT "issue_chat")
