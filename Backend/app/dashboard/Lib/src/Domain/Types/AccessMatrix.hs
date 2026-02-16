{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.AccessMatrix where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement as ProviderAppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement as RiderAppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking as ProviderRideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking as RiderRideBooking
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet as ProviderFleet
import qualified "shared-services" API.Types.ProviderPlatform.IssueManagement as ProviderIssueManagement
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management as ProviderManagement
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Operator as ProviderOperator
import qualified "shared-services" API.Types.RiderPlatform.IssueManagement as RiderIssueManagement
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management as RiderManagement
import qualified Data.Aeson as A
import qualified Data.List
import Data.Singletons.TH
import qualified Data.Text as T
import Domain.Types.Merchant
import Domain.Types.Role as DRole
import Domain.Types.ServerName as DSN
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import qualified Text.Read
import qualified Text.Show

-------- Possible user action for helper API --------

data UserAccessType
  = USER_FULL_ACCESS
  | USER_NO_ACCESS
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

$(mkBeamInstancesForEnum ''UserAccessType)

newtype UserActionTypeWrapper = UserActionTypeWrapper {getUserActionType :: UserActionType}
  deriving newtype (ToSchema, Eq, Ord)

instance Text.Show.Show UserActionTypeWrapper where
  show (UserActionTypeWrapper uat) = case uat of
    PROVIDER_FLEET uat1 -> "PROVIDER_FLEET/" <> show uat1
    PROVIDER_OPERATOR uat1 -> "PROVIDER_OPERATOR/" <> show uat1
    PROVIDER_MANAGEMENT uat1 -> "PROVIDER_MANAGEMENT/" <> show uat1
    PROVIDER_APP_MANAGEMENT uat1 -> "PROVIDER_APP_MANAGEMENT/" <> show uat1
    PROVIDER_ISSUE_MANAGEMENT uat1 -> "PROVIDER_ISSUE_MANAGEMENT/" <> show uat1
    PROVIDER_RIDE_BOOKING uat1 -> "PROVIDER_RIDE_BOOKING/" <> show uat1
    RIDER_MANAGEMENT uat1 -> "RIDER_MANAGEMENT/" <> show uat1
    RIDER_APP_MANAGEMENT uat1 -> "RIDER_APP_MANAGEMENT/" <> show uat1
    RIDER_ISSUE_MANAGEMENT uat1 -> "RIDER_ISSUE_MANAGEMENT/" <> show uat1
    RIDER_RIDE_BOOKING uat1 -> "RIDER_RIDE_BOOKING/" <> show uat1
    _ -> show uat -- TODO remove after move all apis to DSL

instance Text.Read.Read UserActionTypeWrapper where
  readsPrec d' =
    Text.Read.readParen
      (d' > app_prec)
      ( \r ->
          [ (UserActionTypeWrapper $ PROVIDER_FLEET v1, r2)
            | r1 <- stripPrefix "PROVIDER_FLEET/" r,
              (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
          ]
            ++ [ (UserActionTypeWrapper $ PROVIDER_OPERATOR v1, r2)
                 | r1 <- stripPrefix "PROVIDER_OPERATOR/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ PROVIDER_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "PROVIDER_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ PROVIDER_APP_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "PROVIDER_APP_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ PROVIDER_ISSUE_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "PROVIDER_ISSUE_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ PROVIDER_RIDE_BOOKING v1, r2)
                 | r1 <- stripPrefix "PROVIDER_RIDE_BOOKING/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ RIDER_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "RIDER_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ RIDER_APP_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "RIDER_APP_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ RIDER_ISSUE_MANAGEMENT v1, r2)
                 | r1 <- stripPrefix "RIDER_ISSUE_MANAGEMENT/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper $ RIDER_RIDE_BOOKING v1, r2)
                 | r1 <- stripPrefix "RIDER_RIDE_BOOKING/" r,
                   (v1, r2) <- Text.Read.readsPrec (app_prec + 1) r1
               ]
            ++ [ (UserActionTypeWrapper v1, r2)
                 | (v1, r2) <- Text.Read.readsPrec @UserActionType (app_prec + 1) r
               ]
      )
    where
      app_prec = 9
      stripPrefix pref r = bool [] [Data.List.drop (length pref) r] $ Data.List.isPrefixOf pref r

instance FromJSON UserActionTypeWrapper where
  parseJSON = A.withText "userActionType" $ \str -> do
    case Text.Read.readEither @UserActionTypeWrapper (T.unpack str) of
      Left _err -> fail "Could not parse UserActionType"
      Right uat -> pure uat

instance ToJSON UserActionTypeWrapper where
  toJSON = A.String . T.pack . Text.Show.show @UserActionTypeWrapper

-- TODO remove old
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
  | CUSTOMER_CANCELLATION_DUES_SYNC
  | CUSTOMER_CANCELLATION_DUES_DETAILS
  | CANCELLATION_CHARGES_WAIVE_OFF
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
  | ADD_RC_FLEET_WITHOUT_DRIVER
  | GET_ALL_VEHICLE_FOR_FLEET
  | GET_ALL_DRIVERS_FOR_FLEET
  | FLEET_UNLINK_VEHICLE
  | FLEET_REMOVE_VEHICLE
  | FLEET_REMOVE_DRIVER
  | FLEET_STATS
  | FLEET_TOTAL_EARNING
  | FLEET_VEHICLE_EARNING
  | FLEET_DRIVER_EARNING
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
  | CREATE_ISSUE_CATEGORY
  | UPDATE_ISSUE_CATEGORY
  | CREATE_ISSUE_OPTION
  | UPDATE_ISSUE_OPTION
  | UPSERT_ISSUE_MESSAGE
  | UPLOAD_ISSUE_MESSAGE_MEDIA_FILES
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
  | SHARE_RIDE_INFO
  | RIDE_INFO_CUSTOMER
  | CLEAR_ON_RIDE_STUCK_DRIVER_IDS
  | BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE
  | FARE_BREAKUP
  | GET_DRIVER_HOME_LOCATION
  | UPDATE_DRIVER_HOME_LOCATION
  | INCREMENT_DRIVER_GO_TO_COUNT
  | GET_DRIVER_GO_HOME_INFO
  | AADHAAR_INFO_PHONE
  | PERSON_NUMBERS
  | AADHAAR_UPDATE
  | CREATE_FP_DRIVER_EXTRA_FEE
  | UPDATE_FP_DRIVER_EXTRA_FEE
  | UPDATE_FP_PER_EXTRA_KM_RATE
  | UPDATE_FARE_POLICY
  | UPSERT_FARE_POLICY
  | BALANCE_DUE
  | COLLECT_CASH
  | EXEMPT_CASH
  | COLLECT_CASH_V2
  | EXEMPT_CASH_V2
  | CURRENT_ACTIVE_RIDE
  | LIST_PLAN
  | SELECT_PLAN
  | PAYMENT_STATUS
  | SUSPEND_PLAN
  | SUBSCRIBE_PLAN
  | CURRENT_PLAN
  | PAYMENT_HISTORY
  | PAYMENT_HISTORY_ENTITY_DETAILS
  | PAYMENT_HISTORY_V2
  | PAYMENT_HISTORY_ENTITY_DETAILS_V2
  | LIST_PLAN_V2
  | SELECT_PLAN_V2
  | PAYMENT_STATUS_V2
  | SUSPEND_PLAN_V2
  | SUBSCRIBE_PLAN_V2
  | CURRENT_PLAN_V2
  | CREATE_OVERLAY
  | DELETE_OVERLAY
  | LIST_OVERLAY
  | OVERLAY_INFO
  | SCHEDULE_OVERLAY
  | DRIVER_SUBSCRIPTION_DRIVER_FEE_AND_INVOICE_UPDATE
  | GET_ALL_DRIVER_VEHICLE_ASSOCIATION_FOR_FLEET
  | SET_VEHICLE_DRIVER_RC_STATUS_FOR_FLEET
  | GET_DRIVER_VEHICLE_ASSOCIATION
  | GET_DRIVER_ASSOCIATION
  | GET_VEHICLE_ASSOCIATION
  | GET_DRIVER_ASSOCIATION_BY_SEARCH
  | GET_VEHICLE_ASSOCIATION_BY_SEARCH
  | SEND_DASHBOARD_MESSAGE
  | VERIFY_BOOKING_DETAILS
  | GET_TICKET_SERVICES
  | UPDATE_SEAT_MANAGEMENT
  | SEND_DUMMY_NOTIFICATION
  | CHANGE_OPERATING_CITY
  | TOGGLE_SERVICE_USAGE_CHARGE
  | SCHEDULER_TRIGGER
  | DRIVER_COIN_BULK_UPLOAD
  | DRIVER_COIN_BULK_UPLOAD_V2
  | DRIVER_COIN_HISTORY
  | REMOVE_EXPIRED_HOTSPOTS
  | GET_OPERATING_CITY
  | CREATE_MERCHANT_OPERATING_CITY
  | UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING
  | UPSERT_SPECIAL_LOCATION
  | DELETE_SPECIAL_LOCATION
  | UPSERT_SPECIAL_LOCATION_GATE
  | DELETE_SPECIAL_LOCATION_GATE
  | UPDATE_RC_INVALID_STATUS
  | BULK_REVIEW_RC_VARIANT
  | UPDATE_SAFETY_CENTER
  | UPDATE_VEHICLE_VARIANT
  | UNDER_REVIEW_DRIVERS_LIST
  | DRIVER_DOCUMENT_INFO
  | UPDATE_DOCUMENT
  | GET_TICKET_PLACES
  | REMOVE_AC_USAGE_RESTRICTION
  | UPDATE_DRIVER_TAG
  | CANCEL_TICKET_BOOKING
  | CANCEL_TICKET_SERVICE
  | GET_TICKET_BOOKING_DETAILS
  | UPDATE_FLEET_OWNER_INFO
  | GET_FLEET_OWNER_INFO
  | SEND_FLEET_JOINING_OTP
  | VERIFY_FLEET_JOINING_OTP
  | LIST_DRIVER_RIDES
  | LINK_RC_WITH_DRIVER
  | CLEAR_FEE
  | PAN_AADHAAR_SELFIE_DETAILS
  | SYNC_DOC_AADHAR_PAN
  | CREATE_NAMMA_TAG
  | TIME_BOUNDS
  | CREATE_CHAKRA_QUERY
  | MANUAL_TAG_UPDATE
  | RUN_KAAL_CHAKRA_JOB
  | APP_DYNAMIC_LOGIC_VERIFY
  | APP_DYNAMIC_LOGIC_ROLLOUT
  | GET_CHAKRA_QUERY
  | INVOICE
  | FETCH_PERSON_ID
  | BHARAT_TAXI_FROM_LIST
  | BHARAT_TAXI_TO_LIST
  | BHARAT_TAXI_ESTIMATE
  | BHARAT_TAXI_BOOKING
  | BHARAT_TAXI_INVOICE
  | BHARAT_TAXI_BOOKING_LATEST
  | BHARAT_TAXI_BOOKING_BY_ID
  | BHARAT_TAXI_UPDATE_BOOKING
  | BHARAT_TAXI_VEHICLES_LIST
  | BHARAT_TAXI_VEHICLES_CREATE
  | BHARAT_TAXI_DRIVERS_LIST
  | BHARAT_TAXI_DRIVERS_CREATE
  | PAYOUT_MANAGEMENT
  | UPDATE_VEHICLE_MANUFACTURING
  | REFUND_BY_PAYOUT
  | SECURITY_DEPOSIT_STATUS
  | DRIVER_DECRYPTION
  | CLEAR_CACHE_SUBSCRIPTION
  | RUN_QUERY
  | LIST_FRFS_ROUTES
  | ADD_FRFS_ROUTE
  | DELETE_FRFS_ROUTE
  | LIST_FRFS_ROUTE_FARE
  | UPSERT_FRFS_ROUTE_FARE
  | LIST_FRFS_STATION
  | ADD_FRFS_STATION
  | DELETE_FRFS_STATION
  | TOGGLE_CONFIG_PRIORITY
  | COLLECT_MANUAL_PAYMENTS
  | EXEMPT_DRIVER_FEE
  | PAN_AADHAAR_SELFIE_DETAILS_LIST
  | WHITELIST_MERCHANT_OPERATING_CITY
  | PROVIDER_FLEET ProviderFleet.FleetUserActionType
  | PROVIDER_OPERATOR ProviderOperator.OperatorUserActionType
  | PROVIDER_MANAGEMENT ProviderManagement.ManagementUserActionType
  | PROVIDER_APP_MANAGEMENT ProviderAppManagement.AppManagementUserActionType
  | PROVIDER_ISSUE_MANAGEMENT ProviderIssueManagement.IssueManagementUserActionType
  | PROVIDER_RIDE_BOOKING ProviderRideBooking.RideBookingUserActionType
  | RIDER_MANAGEMENT RiderManagement.ManagementUserActionType
  | RIDER_APP_MANAGEMENT RiderAppManagement.AppManagementUserActionType
  | RIDER_ISSUE_MANAGEMENT RiderIssueManagement.IssueManagementUserActionType
  | RIDER_RIDE_BOOKING RiderRideBooking.RideBookingUserActionType
  deriving (Show, Read, Generic, ToSchema, Eq, Ord)

$(mkBeamInstancesForEnum ''UserActionTypeWrapper)

genSingletons [''UserActionType]

-------- Required access levels for helper api --------

-- TODO ApiEntity will be deprecated when we move all apis to DSL. For now we use DSL api entity for all generated apis
data ApiEntity = DSL | CUSTOMERS | DRIVERS | RIDES | MONITORING | MERCHANT | MESSAGE | REFERRAL | ISSUE | VOLUNTEER | SPECIAL_ZONES | SUBSCRIPTION | FLEET | OVERLAY | NAMMA_TAG | MIGRATION | FRFS | BHARAT_TAXI_USER
  deriving (Show, Read, Generic, ToJSON, FromJSON, ToSchema, Eq, Ord)

$(mkBeamInstancesForEnum ''ApiEntity)

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
    userActionType :: UserActionTypeWrapper,
    -- isDerived :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }

data MerchantCityList = MerchantCityList
  { merchantId :: ShortId Merchant,
    cityList :: [City.City]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AccessMatrixAPIEntity = AccessMatrixAPIEntity
  {accessMatrix :: [AccessMatrixRowAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AccessMatrixRowAPIEntity = AccessMatrixRowAPIEntity
  { role :: DRole.RoleAPIEntity,
    accessMatrixRow :: [AccessMatrixItemAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AccessMatrixItemAPIEntity = AccessMatrixItemAPIEntity
  { apiEntity :: ApiEntity,
    userAccessType :: UserAccessType,
    userActionType :: UserActionTypeWrapper
    -- isDerived :: Bool
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
