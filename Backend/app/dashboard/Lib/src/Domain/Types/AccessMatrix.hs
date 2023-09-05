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
  | AADHAAR_INFO
  | LIST
  | ACTIVITY
  | ENABLE
  | DISABLE
  | BLOCK
  | BLOCK_WITH_REASON
  | BLOCK_REASON_LIST
  | UNBLOCK
  | LOCATION
  | INFO
  | DELETE_DRIVER
  | UNLINK_VEHICLE
  | END_RC_ASSOCIATION
  | SET_RC_STATUS
  | DELETE_RC
  | UNLINK_DL
  | UNLINK_AADHAAR
  | UPDATE_PHONE_NUMBER
  | ADD_VEHICLE
  | ADD_VEHICLE_FLEET
  | GET_ALL_VEHICLE_FOR_FLEET
  | FLEET_UNLINK_VEHICLE
  | FLEET_REMOVE_VEHICLE
  | FLEET_STATS
  | UPDATE_DRIVER_NAME
  | STUCK_BOOKING_CANCEL
  | REFERRAL_PROGRAM_PASSWORD_UPDATE
  | REFERRAL_PROGRAM_LINK_CODE
  | ISSUE_LIST
  | ISSUE_CATEGORY_LIST
  | ISSUE_INFO
  | ISSUE_UPDATE
  | ISSUE_ADD_COMMENT
  | ISSUE_FETCH_MEDIA
  | TICKET_STATUS_CALL_BACK
  | MERCHANT_UPDATE
  | MERCHANT_COMMON_CONFIG
  | MERCHANT_COMMON_CONFIG_UPDATE
  | DRIVER_POOL_CONFIG
  | DRIVER_POOL_CONFIG_UPDATE
  | DRIVER_POOL_CONFIG_CREATE
  | DRIVER_INTELLIGENT_POOL_CONFIG
  | DRIVER_INTELLIGENT_POOL_CONFIG_UPDATE
  | ONBOARDING_DOCUMENT_CONFIG
  | ONBOARDING_DOCUMENT_CONFIG_UPDATE
  | ONBOARDING_DOCUMENT_CONFIG_CREATE
  | SERVICE_USAGE_CONFIG
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
  | TICKET_RIDE_LIST_API
  | RIDE_ROUTE
  | RIDE_START
  | RIDE_END
  | MULTIPLE_RIDE_END
  | RIDE_CANCEL
  | MULTIPLE_RIDE_CANCEL
  | RIDE_INFO
  | RIDE_SYNC
  | MULTIPLE_RIDE_SYNC
  | CUSTOMER_LIST
  | CUSTOMER_BLOCK
  | CUSTOMER_UNBLOCK
  | CUSTOMER_DELETE
  | CUSTOMER_INFO
  | DOCUMENT_LIST
  | GET_DOCUMENT
  | UPLOAD_DOCUMENT
  | REGISTER_DL
  | REGISTER_RC
  | GENERATE_AADHAAR_OTP
  | VERIFY_AADHAAR_OTP
  | VOLUNTEER_BOOKING_INFO
  | VOLUNTEER_ASSIGN_CREATE_AND_START_OTP_RIDE
  | VOLUNTEER_COLLECTION_HISTORY
  | ALL_FEE_HISTORY
  | BOOKING_STATUS
  | BOOKINGLIST
  | CONFIRM
  | AUTOCOMPLETE
  | PLACEDETAIL
  | PLACENAME
  | PERSONDETAIL
  | UPDATEPERSON
  | GETQUOTE
  | AUTH
  | VERIFY
  | RESEND
  | LOGOUT
  | SEARCH
  | SELECT
  | SELECTLIST
  | SELECTRESULT
  | CANCELSEARCH
  | FLOW_STATUS
  | NOTIFYEVENT
  | CANCEL_BOOKING
  | MULTIPLE_BOOKING_SYNC
  | SPECIAL_ZONE_CREATE
  | SPECIAL_ZONE_DELETE
  | SPECIAL_ZONE_UPDATE
  | SPECIAL_ZONE_LOOKUP
  | LIST_ISSUE
  | TRIP_ROUTE
  | RIDE_INFO_CUSTOMER
  | CLEAR_ON_RIDE_STUCK_DRIVER_IDS
  | BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE
  | GET_DRIVER_HOME_LOCATION
  | UPDATE_DRIVER_HOME_LOCATION
  | INCREMENT_DRIVER_GO_TO_COUNT
  | AADHAAR_INFO_PHONE
  | AADHAAR_UPDATE
  | CREATE_FP_DRIVER_EXTRA_FEE
  | UPDATE_FP_DRIVER_EXTRA_FEE
  | BALANCE_DUE
  | COLLECT_CASH
  | EXEMPT_CASH
  | CURRENT_ACTIVE_RIDE
  | LIST_PLAN
  | SELECT_PLAN
  | SUSPEND_PLAN
  | SUBSCRIBE_PLAN
  | CURRENT_PLAN
  | UPDATE_REGISTRY_MAP
  | PAYMENT_HISTORY
  | PAYMENT_HISTORY_ENTITY_DETAILS
  | CREATE_OVERLAY
  | DELETE_OVERLAY
  | LIST_OVERLAY
  | OVERLAY_INFO
  | SCHEDULE_OVERLAY
  | DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema)

genSingletons [''UserActionType]

-------- Required access levels for helper api --------

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING | MERCHANT | MESSAGE | REFERRAL | ISSUE | VOLUNTEER | SPECIAL_ZONES | SUBSCRIPTION | ADMIN | FLEET | OVERLAY
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
