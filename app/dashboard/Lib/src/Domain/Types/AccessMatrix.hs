{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix where

import Beckn.Prelude
import Beckn.Types.Id
import Data.Singletons.TH
import Domain.Types.Role as DRole

-------- Possible user access levels for helper API --------

data UserAccessType
  = USER_READ_ACCESS
  | USER_WRITE_ACCESS
  | USER_FULL_ACCESS
  | USER_NO_ACCESS
  deriving (Show, Read)

-------- Required access levels for helper api --------

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING deriving (Show, Read)

genSingletons [''ApiEntity]

data ApiAccessLevel = ApiAccessLevel
  { apiAccessType :: ApiAccessType,
    apiEntity :: ApiEntity
  }

-------- Required access levels for any api --------

data RequiredAccessLevel
  = RequiredApiAccessLevel ApiAccessLevel
  | RequiredDashboardAccessLevel DRole.DashboardAccessType

-------- Access Matrix item --------

-- roleId & apiEntity should be unique
-- if there is no AccessMatrix item, then we can use USER_NO_ACCESS by default
data AccessMatrix = AccessMatrix
  { id :: Id AccessMatrix,
    roleId :: Id DRole.Role,
    apiEntity :: ApiEntity,
    userAccessType :: UserAccessType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
