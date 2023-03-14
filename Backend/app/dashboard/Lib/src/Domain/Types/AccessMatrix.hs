{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.AccessMatrix where

import Data.Singletons.TH
import Domain.Types.Role as DRole
import Domain.Types.ServerName as DSN
import Kernel.Prelude
import Kernel.Types.Id

-------- Possible user access levels for helper API --------

data UserAccessType
  = USER_READ_ACCESS
  | USER_WRITE_ACCESS
  | USER_FULL_ACCESS
  | USER_NO_ACCESS
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-------- Required access levels for helper api --------

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING | MERCHANT | MESSAGE | REFERRAL | ISSUE
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''ApiEntity]

data ApiAccessLevel = ApiAccessLevel
  { serverName :: DSN.ServerName,
    apiAccessType :: ApiAccessType,
    apiEntity :: ApiEntity
  }

-------- Access Matrix item --------

-- roleId & apiEntity should be unique
-- if there is no AccessMatrix item, then we can use USER_NO_ACCESS by default
data AccessMatrixItem = AccessMatrixItem
  { id :: Id AccessMatrixItem,
    roleId :: Id DRole.Role,
    apiEntity :: ApiEntity,
    userAccessType :: UserAccessType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

newtype AccessMatrixAPIEntity = AccessMatrixAPIEntity
  {accessMatrix :: [AccessMatrixRowAPIEntity]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AccessMatrixRowAPIEntity = AccessMatrixRowAPIEntity
  { role :: DRole.RoleAPIEntity,
    accessMatrixRow :: [AccessMatrixItemAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AccessMatrixItemAPIEntity = AccessMatrixItemAPIEntity
  { apiEntity :: ApiEntity,
    userAccessType :: UserAccessType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

mkAccessMatrixAPIEntity :: [DRole.Role] -> [AccessMatrixItem] -> AccessMatrixAPIEntity
mkAccessMatrixAPIEntity roles accessMatrixItems =
  AccessMatrixAPIEntity
    { accessMatrix = mkAccessMatrixRowAPIEntity accessMatrixItems <$> roles
    }

mkAccessMatrixRowAPIEntity :: [AccessMatrixItem] -> DRole.Role -> AccessMatrixRowAPIEntity
mkAccessMatrixRowAPIEntity items role = do
  let filteredItems = filter (\item -> item.roleId == role.id) items
  AccessMatrixRowAPIEntity
    { role = mkRoleAPIEntity role,
      accessMatrixRow = mkAccessMatrixItemAPIEntity <$> filteredItems
    }

mkAccessMatrixItemAPIEntity :: AccessMatrixItem -> AccessMatrixItemAPIEntity
mkAccessMatrixItemAPIEntity AccessMatrixItem {..} = AccessMatrixItemAPIEntity {..}
