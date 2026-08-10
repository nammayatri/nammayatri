{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Capability where

import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Id

-- Capability framework (dashboard unification). Capabilities are THE
-- authorization unit; roles bundle capabilities; person_capability rows are
-- per-user overrides (DENY wins over role grants). Tools.Auth.Capability
-- enforces these tables on every request with no access_matrix fallback.

data CapabilityMode = GRANT | DENY
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''CapabilityMode)

data Capability = Capability
  { id :: Id Capability,
    domain :: Text,
    description :: Text,
    isSystem :: Bool,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data CapabilityEndpoint = CapabilityEndpoint
  { capabilityId :: Id Capability,
    serverName :: Text,
    endpointId :: Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RoleCapability = RoleCapability
  { roleId :: Id DRole.Role,
    capabilityId :: Id Capability,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data PersonCapability = PersonCapability
  { personId :: Id DP.Person,
    capabilityId :: Id Capability,
    mode :: CapabilityMode,
    reason :: Text,
    grantedBy :: Maybe (Id DP.Person),
    expiresAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data AccessAudit = AccessAudit
  { id :: Id AccessAudit,
    actorId :: Maybe (Id DP.Person),
    action :: Text,
    targetType :: Text,
    targetId :: Maybe Text,
    beforeValue :: Maybe Text,
    afterValue :: Maybe Text,
    reason :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Admin tiers as stored in person.admin_tier. Kept as constants over Text
-- (not an enum column type) so the narrow PersonTier projection stays
-- decoupled from the full Person record until the Phase 4 flip.
superAdminTier :: Text
superAdminTier = "SUPER_ADMIN"

dashboardAdminTier :: Text
dashboardAdminTier = "DASHBOARD_ADMIN"

userTier :: Text
userTier = "USER"

data PersonTier = PersonTier
  { id :: Id DP.Person,
    adminTier :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
