{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Storage.Beam.Issue.IssueMessage where

import Data.Serialize hiding (label)
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import IssueManagement.Tools.UtilsTH
import Kernel.Prelude hiding (Generic)
import Sequelize

data IssueMessageT f = IssueMessageT
  { id :: B.C f Text,
    categoryId :: B.C f (Maybe Text),
    optionId :: B.C f (Maybe Text),
    message :: B.C f Text,
    priority :: B.C f Int,
    label :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueMessageT where
  data PrimaryKey IssueMessageT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey :: IssueMessageT column -> B.PrimaryKey IssueMessageT column
  primaryKey = Id . id

type IssueMessage = IssueMessageT Identity

$(enableKVPG ''IssueMessageT ['id] [['message]])

$(mkTableInstancesGenericSchema ''IssueMessageT "issue_message")
