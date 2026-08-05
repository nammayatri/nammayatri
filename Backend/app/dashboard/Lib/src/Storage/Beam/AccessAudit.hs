{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.AccessAudit where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data AccessAuditT f = AccessAuditT
  { id :: B.C f Text,
    actorId :: B.C f (Maybe Text),
    action :: B.C f Text,
    targetType :: B.C f Text,
    targetId :: B.C f (Maybe Text),
    beforeValue :: B.C f (Maybe Text),
    afterValue :: B.C f (Maybe Text),
    reason :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AccessAuditT where
  data PrimaryKey AccessAuditT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type AccessAudit = AccessAuditT Identity

$(enableKVPG ''AccessAuditT ['id] [])

$(mkTableInstancesGenericSchema ''AccessAuditT "access_audit")
