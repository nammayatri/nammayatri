{-# LANGUAGE StandaloneKindSignatures #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Role where

import Data.Singletons.TH
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Id

-------- Required access levels for dashboard api --------

-- DASHBOARD_ADMIN is superuser, who can can create and assign other roles

data DashboardAccessType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord)

$(mkBeamInstancesForEnum ''DashboardAccessType)

genSingletons [''DashboardAccessType]

-------- Person Role --------

data Role = Role
  { id :: Id Role,
    name :: Text,
    dashboardAccessType :: DashboardAccessType,
    description :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

data RoleAPIEntity = RoleAPIEntity
  { id :: Id Role,
    name :: Text,
    dashboardAccessType :: DashboardAccessType,
    description :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mkRoleAPIEntity :: Role -> RoleAPIEntity
mkRoleAPIEntity Role {..} = RoleAPIEntity {..}
