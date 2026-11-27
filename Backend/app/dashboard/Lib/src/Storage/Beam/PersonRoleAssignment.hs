{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.PersonRoleAssignment where

import qualified Data.Time as Time
import qualified Database.Beam as B
import qualified Domain.Types.PersonRoleAssignment as DPRA
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data PersonRoleAssignmentT f = PersonRoleAssignmentT
  { id :: B.C f Text,
    personId :: B.C f Text,
    roleId :: B.C f Text,
    scopeType :: B.C f DPRA.ScopeType,
    scopeId :: B.C f Text,
    merchantId :: B.C f Text,
    status :: B.C f DPRA.AssignmentStatus,
    grantedBy :: B.C f (Maybe Text),
    manuallyGranted :: B.C f Bool,
    handoverFrom :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonRoleAssignmentT where
  data PrimaryKey PersonRoleAssignmentT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PersonRoleAssignment = PersonRoleAssignmentT Identity

$(enableKVPG ''PersonRoleAssignmentT ['id] [])

$(mkTableInstancesGenericSchema ''PersonRoleAssignmentT "person_role_assignment")
