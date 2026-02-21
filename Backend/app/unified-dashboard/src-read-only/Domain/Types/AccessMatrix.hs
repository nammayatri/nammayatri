{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AccessMatrix where

import Data.Aeson
import qualified Domain.Types.Role
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Dhall
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data AccessMatrix = AccessMatrix
  { additionalUserActions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AccessMatrix.AccessMatrix,
    roleId :: Kernel.Types.Id.Id Domain.Types.Role.Role,
    serverName :: Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName,
    updatedAt :: Kernel.Prelude.UTCTime,
    userActionType :: Domain.Types.AccessMatrix.UserActionType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ApiAccessLevel = ApiAccessLevel {serverName :: Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName, userActionType :: Domain.Types.AccessMatrix.UserActionType}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord)

data ServerName
  = APP_BACKEND
  | APP_BACKEND_MANAGEMENT
  | DRIVER_OFFER_BPP
  | DRIVER_OFFER_BPP_MANAGEMENT
  | SPECIAL_ZONE
  | BHARAT_TAXI
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

data UserActionType
  = POST_USER_LOGIN
  | POST_USER_LOGIN_OTP
  | POST_USER_LOGIN_VERIFY
  | POST_USER_ENABLE_2FA
  | POST_MANAGEMENT_PERSON_CREATE
  | GET_MANAGEMENT_ACCESS_MATRIX
  | GET_MANAGEMENT_MERCHANT_WITH_CITY_LIST
  | CREATE_MANAGEMENT_MERCHANT_WITH_ADMIN
  | CREATE_MANAGEMENT_MERCHANT
  | LIST_MANAGEMENT_MERCHANTS
  | CHANGE_MANAGEMENT_MERCHANT_ENABLE_STATE
  | GET_MANAGEMENT_PERSON_LIST
  | POST_MANAGEMENT_PERSON_ASSIGN_ROLE
  | POST_MANAGEMENT_PERSON_ASSIGN_MERCHANT_CITY_ACCESS
  | POST_MANAGEMENT_PERSON_RESET_MERCHANT_ACCESS
  | POST_MANAGEMENT_PERSON_RESET_MERCHANT_CITY_ACCESS
  | DELETE_MANAGEMENT_PERSON
  | POST_MANAGEMENT_PERSON_CHANGE_ENABLED_STATUS
  | POST_MANAGEMENT_PERSON_CHANGE_EMAIL_BY_ADMIN
  | POST_MANAGEMENT_PERSON_CHANGE_PASSWORD_BY_ADMIN
  | POST_MANAGEMENT_PERSON_CHANGE_MOBILE_BY_ADMIN
  | GET_MANAGEMENT_USER_PROFILE
  | GET_MANAGEMENT_USER_CURRENT_MERCHANT
  | POST_MANAGEMENT_USER_CHANGE_PASSWORD
  | GET_MANAGEMENT_USER_ACCESS_MATRIX
  | POST_MANAGEMENT_USER_CHANGE_PASSWORD_AFTER_EXPIRY
  | POST_MANAGEMENT_ROLE_CREATE
  | POST_MANAGEMENT_ROLE_ASSIGN_ACCESS_LEVEL
  | GET_MANAGEMENT_ROLE_LIST
  | GET_MANAGEMENT_TRANSACTION_LIST
  | GET_FLEET_HEALTH_CHECK_TEST
  | GET_OPERATOR_HEALTH_CHECK_TEST
  | GET_RIDE_BOOKING_HEALTH_CHECK_TEST
  | GET_MANAGEMENT_HEALTH_CHECK_TEST
  | CONTROL_CENTER_ACCESS
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ServerName)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''UserActionType)

$(mkHttpInstancesForEnum ''UserActionType)
