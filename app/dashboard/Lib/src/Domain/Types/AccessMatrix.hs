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
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-------- Required access levels for helper api --------

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''ApiEntity]

data ApiAccessLevel = ApiAccessLevel
  { apiAccessType :: ApiAccessType,
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
