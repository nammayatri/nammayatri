{-# LANGUAGE StandaloneKindSignatures #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Role where

import Data.Singletons.TH
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Types.Id

-------- Required access levels for dashboard api --------

-- DASHBOARD_ADMIN is superuser, who can can create and assign other roles

data DashboardAccessType = DASHBOARD_USER | DASHBOARD_ADMIN | FLEET_OWNER | DASHBOARD_RELEASE_ADMIN | MERCHANT_ADMIN | RENTAL_FLEET_OWNER | MERCHANT_MAKER | MERCHANT_SERVER | DASHBOARD_OPERATOR | TICKET_DASHBOARD_USER | TICKET_DASHBOARD_MERCHANT | TICKET_DASHBOARD_ADMIN | TICKET_DASHBOARD_APPROVER
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord)

$(mkBeamInstancesForEnum ''DashboardAccessType)

genSingletons [''DashboardAccessType]

-------- Two-factor delivery mode --------

-- | The second-factor delivery mechanism for a dashboard login. Persisted on both Person and
-- Role; MOBILE_OTP on either forces the mobile path (see Registration.resolve2FAMode).
--
--   * AUTHENTICATOR_APP (default): TOTP via an authenticator app, enrolled through the email-OTP
--     setup flow and recorded by Person.secretKey.
--   * MOBILE_OTP: a one-time code texted to the person's registered mobile number at login.
data TwoFaMode = AUTHENTICATOR_APP | MOBILE_OTP
  deriving (Show, Read, Eq, Ord, Generic, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''TwoFaMode)

-------- Person Role --------

data Role = Role
  { id :: Id Role,
    name :: Text,
    dashboardAccessType :: DashboardAccessType,
    description :: Text,
    accessibleRoles :: [Id Role],
    isBppSyncNeeded :: Maybe Bool,
    isDisabled :: Maybe Bool,
    -- | Role-level second-factor delivery. MOBILE_OTP forces the SMS path for every holder of the
    -- role; AUTHENTICATOR_APP (default) leaves each person's own twoFaMode in effect. Only a
    -- SUPER_ADMIN may change it (Registration.setTwoFaMode, keyed by role name).
    twoFaMode :: TwoFaMode,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RoleAPIEntity = RoleAPIEntity
  { id :: Id Role,
    name :: Text,
    dashboardAccessType :: DashboardAccessType,
    description :: Text,
    isBppSyncNeeded :: Maybe Bool,
    isDisabled :: Maybe Bool,
    twoFaMode :: TwoFaMode
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mkRoleAPIEntity :: Role -> RoleAPIEntity
mkRoleAPIEntity Role {..} = RoleAPIEntity {..}

isBppSyncRole :: Role -> Bool
isBppSyncRole Role {..} = isBppSyncNeeded == Just True
