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

-------- Possible user action for helper API --------

data UserAccessType
  = USER_FULL_ACCESS
  | USER_NO_ACCESS
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data UserActionType
  = DOCUMENTS_INFO
  | LIST
  | ACTIVITY
  | ENABLE
  | DISABLE
  | BLOCK
  | UNBLOCK
  | LOCATION
  | INFO
  | DELETE_DRIVER
  | UNLINK_VEHICLE
  | END_RC_ASSOCIATION
  | UNLINK_DL
  | UPDATE_PHONE_NUMBER
  | ADD_VEHICLE
  | UPDATE_DRIVER_NAME
  | STUCK_BOOKING_CANCEL
  | REFERRAL_PROGRAM_PASSWORD_UPDATE
  | REFERRAL_PROGRAM_LINK_CODE
  | ISSUE_LIST
  | ISSUE_UPDATE
  | ISSUE_ADD_COMMENT
  | MERCHANT_UPDATE
  | MAPS_SERVICE_CONFIG_UPDATE
  | MAPS_SERVICE_USAGE_CONFIG_UPDATE
  | SMS_SERVICE_CONFIG_UPDATE
  | SMS_SERVICE_USAGE_CONFIG_UPDATE
  | VERIFICATION_SERVICE_CONFIG_UPDATE
  | UPLOAD_FILE
  | ADD_LINK
  | ADD_MESSAGE
  | SEND_MESSAGE
  | MESSAGE_LIST
  | MESSAGE_INFO
  | MESSAGE_DELIVERY_INFO
  | MESSAGE_RECEIVER_LIST
  | RIDE_LIST
  | RIDE_ROUTE
  | RIDE_START
  | RIDE_END
  | RIDE_CANCEL
  | RIDE_INFO
  | RIDE_SYNC
  | CUSTOMER_LIST
  | CUSTOMER_UPDATE
  | CUSTOMER_DELETE
  | DOCUMENT_LIST
  | GET_DOCUMENT
  | UPLOAD_DOCUMENT
  | REGISTER_DL
  | REGISTER_RC
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''UserActionType]

-------- Required access levels for helper api --------

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING | MERCHANT | MESSAGE | REFERRAL | ISSUE
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''ApiEntity]

data ApiAccessLevel = ApiAccessLevel
  { serverName :: DSN.ServerName,
    apiEntity :: ApiEntity,
    userActionType :: UserActionType
  }

-------- Access Matrix item --------

-- roleId & apiEntity should be unique
-- if there is no AccessMatrix item, then we can use USER_NO_ACCESS by default
data AccessMatrixItem = AccessMatrixItem
  { id :: Id AccessMatrixItem,
    roleId :: Id DRole.Role,
    apiEntity :: ApiEntity,
    userAccessType :: UserAccessType,
    userActionType :: UserActionType,
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
    userAccessType :: UserAccessType,
    userActionType :: UserActionType
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
