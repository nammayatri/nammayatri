{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.RoleCapability where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data RoleCapabilityT f = RoleCapabilityT
  { roleId :: B.C f Text,
    capabilityId :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RoleCapabilityT where
  data PrimaryKey RoleCapabilityT f
    = RoleCapabilityKey (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey RoleCapabilityT {..} = RoleCapabilityKey roleId capabilityId

type RoleCapability = RoleCapabilityT Identity

$(enableKVPG ''RoleCapabilityT ['roleId, 'capabilityId] [])

$(mkTableInstancesGenericSchema ''RoleCapabilityT "role_capability")
