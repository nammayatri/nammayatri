{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.DeletedUser where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data DeletedUserT f = DeletedUserT
  { id :: B.C f Text,
    personId :: B.C f Text,
    firstName :: B.C f Text,
    lastName :: B.C f Text,
    roleId :: B.C f Text,
    emailEncrypted :: B.C f (Maybe Text),
    deleteReason :: B.C f (Maybe Text),
    deletedBy :: B.C f Text,
    deletedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DeletedUserT where
  data PrimaryKey DeletedUserT f
    = DeletedUserKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey DeletedUserT {..} = DeletedUserKey id

type DeletedUser = DeletedUserT Identity

$(enableKVPG ''DeletedUserT ['id] [])

$(mkTableInstancesGenericSchema ''DeletedUserT "deleted_user")
