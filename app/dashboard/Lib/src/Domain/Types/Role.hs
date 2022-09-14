{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Role where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Singletons.TH

-------- Required access levels for dashboard api --------

-- DASHBOARD_ADMIN is superuser, who can can create and assign other roles

data DashboardAccessType = DASHBOARD_USER | DASHBOARD_ADMIN
  deriving (Show, Read, Eq)

genSingletons [''DashboardAccessType]

-------- Person Role --------

data Role = Role
  { id :: Id Role, -- do we need it?
    name :: Text, -- should be unique
    dashboardAccessType :: DashboardAccessType,
    description :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
