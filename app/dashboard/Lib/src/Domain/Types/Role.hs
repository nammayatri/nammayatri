{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Role where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Singletons.TH

-------- Required access levels for dashboard api --------

-- DASHBOARD_ADMIN is superuser, who can can create and assign other roles

data DashboardAccessType = DASHBOARD_USER | DASHBOARD_ADMIN
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema)

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
