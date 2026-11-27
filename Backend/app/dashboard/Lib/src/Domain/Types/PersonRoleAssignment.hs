{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.PersonRoleAssignment where

import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Id

-- | What a role assignment is scoped to. A person can hold several assignments, which is what
-- lets a fleet be a scope rather than a login. Stored as the constructor name, so a new kind of
-- scope needs a constructor here and no migration.
data ScopeType
  = MERCHANT
  | CITY
  | FLEET
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''ScopeType)

-- | Only ACTIVE grants access. INVITED is offered and not yet accepted; REVOKED is kept for history.
data AssignmentStatus
  = INVITED
  | ACTIVE
  | REVOKED
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''AssignmentStatus)

data PersonRoleAssignment = PersonRoleAssignment
  { id :: Id PersonRoleAssignment,
    personId :: Id DPerson.Person,
    roleId :: Id DRole.Role,
    scopeType :: ScopeType,
    -- | A merchant id, a city name or a fleet id, depending on 'scopeType'.
    scopeId :: Text,
    merchantId :: Id DMerchant.Merchant,
    status :: AssignmentStatus,
    grantedBy :: Maybe (Id DPerson.Person),
    -- | An admin gave this city or merchant role on its own, beside the person's own role. The sync leaves
    -- such a row alone while the person can still reach its scope. 'grantedBy' cannot say this: it is
    -- cleared when the person who granted is deleted.
    manuallyGranted :: Bool,
    -- | On accept, this person's active assignment on the same fleet is revoked.
    handoverFrom :: Maybe (Id DPerson.Person),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)
