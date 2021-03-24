{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error.Domain where

import Beckn.TypeClass.IsDomainError
import EulerHS.Prelude

data AuthError
  = Unauthorized
  | InvalidAuthData
  | TokenExpired
  | InvalidToken
  | AuthBlocked
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError AuthError where
  toError Unauthorized = APIError "UNAUTHORIZED" "Unauthorized action."
  toError InvalidAuthData = APIError "INVALID_AUTH_DATA" "Authentication data is not valid."
  toError TokenExpired = APIError "TOKEN_EXPIRED" "Token expired."
  toError InvalidToken = APIError "INVALID_TOKEN" "Invalid registration token."
  toError AuthBlocked = APIError "AUTH_BLOCKED" "Authentication process blocked."
  toStatusCode Unauthorized = E401
  toStatusCode InvalidAuthData = E400
  toStatusCode TokenExpired = E400
  toStatusCode InvalidToken = E401
  toStatusCode AuthBlocked = E400

data VehicleError
  = VehicleNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError VehicleError where
  toError VehicleNotFound = APIError "VEHICLE_NOT_FOUND" "Vehicle not found."
  toStatusCode VehicleNotFound = E400

data PersonError
  = PersonNotFound
  | PersonDoesNotExist
  | PersonFieldNotPresent
  | PersonLocationIdNotPresent
  | PersonOrgIdNotPresent
  | PersonOrgExists
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError PersonError where
  toError PersonNotFound = APIError "PERSON_NOT_FOUND" "Person not found."
  toError PersonDoesNotExist = APIError "PERSON_DOES_NOT_EXISTS" "No person matches passed data."
  toError PersonFieldNotPresent = APIError "PERSON_NVALID_STATE" "Required field is null for this person."
  toError PersonLocationIdNotPresent = APIError "PERSON_LOCATION_ID_NOT_PRESENT" "_locationId field is null in this person."
  toError PersonOrgIdNotPresent = APIError "PERSON_ORG_ID_NOT_PRESENT" "_organizationId field is null in this person."
  toError PersonOrgExists = APIError "PERSON_ORG_ALREADY_EXISTS" "Person already registered an organization."
  toStatusCode PersonNotFound = E500
  toStatusCode PersonDoesNotExist = E400
  toStatusCode PersonFieldNotPresent = E500
  toStatusCode PersonLocationIdNotPresent = E500
  toStatusCode PersonOrgIdNotPresent = E500
  toStatusCode PersonOrgExists = E400

data LocationError
  = LocationNotFound
  | LocationDoesNotExist
  | LocationFieldNotPresent
  | LocationLongLatNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError LocationError where
  toError LocationNotFound = APIError "LOCATION_NOT_FOUND" "Location not found."
  toError LocationDoesNotExist = APIError "LOCATION_DOES_NOT_EXISTS" "No location matches passed data."
  toError LocationFieldNotPresent = APIError "LOCATION_FIELD_NOT_PRESENT" "Required field is null for this location."
  toError LocationLongLatNotFound = APIError "LOCATION_LONG_LAT_NOT_FOUND" "_long or _lat field is null for this location."
  toStatusCode LocationNotFound = E500
  toStatusCode LocationDoesNotExist = E400
  toStatusCode LocationFieldNotPresent = E500
  toStatusCode LocationLongLatNotFound = E500

data RouteError
  = UnableToGetRoute
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError RouteError where
  toError UnableToGetRoute = APIError "UNABLE_TO_GET_ROUTE" "Unable to get route."
  toStatusCode UnableToGetRoute = E400

data AmbiguousError
  = UnknownError
  | CommonInternalError
  | UnexpectedError
  | InvalidRequest
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError AmbiguousError where
  toError UnknownError = APIError "UNKNOWN_ERROR" "Something unknown happened."
  toError CommonInternalError = APIError "COMMON_INTERNAL_ERROR" "Something common happened in workflow."
  toError UnexpectedError = APIError "UNEXPECTED_ERROR" "Happened something, that shouldn't be happen at all."
  toError InvalidRequest = APIError "API_REQUEST_ERROR" "Not enough data to complete request."
  toStatusCode UnknownError = E500
  toStatusCode CommonInternalError = E500
  toStatusCode UnexpectedError = E500
  toStatusCode InvalidRequest = E400

data OrganizationError
  = OrgNotFound
  | OrgDoesNotExist
  | OrgFieldNotPresent
  | OrgCallbackUrlNotSet
  | OrgCallbackApiKeyNotSet
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError OrganizationError where
  toError OrgNotFound = APIError "ORGANIZATION_NOT_FOUND" "Organization not found."
  toError OrgDoesNotExist = APIError "ORGANIZATION_DOES_NOT_EXISTS" "No organization matches passed data."
  toError OrgFieldNotPresent = APIError "ORGANIZATION_FIELD_NOT_PRESENT" "Required field is null for this organization."
  toError OrgCallbackUrlNotSet = APIError "ORGANIZATION_CALLBACK_URL_NOT_SET" "Callback url for organization is not set."
  toError OrgCallbackApiKeyNotSet = APIError "ORGANIZATION_CALLBACK_API_KEY_NOT_SET" "Callback api key for organization is not set."
  toStatusCode OrgNotFound = E500
  toStatusCode OrgDoesNotExist = E400
  toStatusCode OrgFieldNotPresent = E500
  toStatusCode OrgCallbackUrlNotSet = E500
  toStatusCode OrgCallbackApiKeyNotSet = E500

data CaseError
  = CaseNotFound
  | CaseDoesNotExist
  | CaseExpired
  | CaseInvalidStatus
  | CaseFieldNotPresent
  | CaseVehicleVariantNotPresent
  | CaseBapOrgIdNotPresent
  | CaseRequestorNotPresent
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError CaseError where
  toError CaseNotFound = APIError "CASE_NOT_FOUND" "Case with this id is not found."
  toError CaseDoesNotExist = APIError "CASE_DOES_NOT_EXISTS" "No case matches passed data."
  toError CaseExpired = APIError "CASE_EXPIRED" "This case expired and no longer valid."
  toError CaseFieldNotPresent = APIError "CASE_FIELD_NOT_PRESENT" "Required field is null for this case."
  toError CaseInvalidStatus = APIError "CASE_INVALID_STATUS" "Attempted to do some action in wrong case status."
  toError CaseVehicleVariantNotPresent = APIError "CASE_VEHICLE_VARIANT_NOT_PRESENT" "_udf1 field is null for this case. Vehicle variant is not set."
  toError CaseBapOrgIdNotPresent = APIError "CASE_BAP_ORG_ID_NOT_PRESENT" "_udf4 field is null for this case. Bap org id is not set."
  toError CaseRequestorNotPresent = APIError "CASE_REQUESTOR_NOT_PRESENT" "_requestor field is null for this case."
  toStatusCode CaseNotFound = E500
  toStatusCode CaseDoesNotExist = E400
  toStatusCode CaseExpired = E400
  toStatusCode CaseFieldNotPresent = E500
  toStatusCode CaseInvalidStatus = E400
  toStatusCode CaseVehicleVariantNotPresent = E500
  toStatusCode CaseBapOrgIdNotPresent = E500
  toStatusCode CaseRequestorNotPresent = E500

data ProductInstanceError
  = PINotFound
  | PIDoesNotExist
  | PIInfoNotPresent
  | PIPersonNotPresent
  | PIParentIdNotPresent
  | PIFromLocationIdNotPresent
  | PIToLocationIdNotPresent
  | PIOTPNotPresent
  | PIFieldNotPresent
  | PIInvalidStatus
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError ProductInstanceError where
  toError PINotFound = APIError "PI_NOT_FOUND" "Product instance not found."
  toError PIDoesNotExist = APIError "PI_DOES_NOT_EXISTS" "No product instance matches passed data."
  toError PIInfoNotPresent = APIError "PI_INFO_NOT_PRESENT" "_info field is null for this product instance."
  toError PIPersonNotPresent = APIError "PI_PERSON_NOT_PRESENT" "_personId field is null for this product instance."
  toError PIParentIdNotPresent = APIError "PI_PARENT_ID_NOT_PRESENT" "_parentId field is null for this product instance."
  toError PIFromLocationIdNotPresent = APIError "PI_FROM_LOCATION_ID_NOT_PRESENT" "_fromLocation field is null for this product instance."
  toError PIToLocationIdNotPresent = APIError "PI_TO_LOCATION_ID_NOT_PRESENT" "_toLocation field is null for this product instance."
  toError PIOTPNotPresent = APIError "PI_OTP_NOT_PRESENT" "_udf4 field is null for this product instance. OTP is not present."
  toError PIFieldNotPresent = APIError "PI_FIELD_NOT_PRESENT" "Required field is null for this product instance."
  toError PIInvalidStatus = APIError "PI_INVALID_STATUS" "Attempted to do some action in wrong product instance status."
  toStatusCode PINotFound = E500
  toStatusCode PIDoesNotExist = E400
  toStatusCode PIInfoNotPresent = E500
  toStatusCode PIPersonNotPresent = E500
  toStatusCode PIParentIdNotPresent = E500
  toStatusCode PIFromLocationIdNotPresent = E500
  toStatusCode PIToLocationIdNotPresent = E500
  toStatusCode PIOTPNotPresent = E500
  toStatusCode PIFieldNotPresent = E500
  toStatusCode PIInvalidStatus = E400

data GatewayError
  = GatewaySelectorNotSet
  | NSDLBaseUrlNotSet
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError GatewayError where
  toError GatewaySelectorNotSet = APIError "GATEWAY_SELECTOR_NOT_SET" "Gateway selector is not set."
  toError NSDLBaseUrlNotSet = APIError "NSDL_BASEURL_NOT_SET" "NSDL base url is not set."
  toStatusCode GatewaySelectorNotSet = E500
  toStatusCode NSDLBaseUrlNotSet = E500

data CommunicationError
  = UnableToSendSMS
  | UnableToCall
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError CommunicationError where
  toError UnableToSendSMS = APIError "UNABLE_TO_SEND_SMS" "Unable to send SMS."
  toError UnableToCall = APIError "UNABLE_TO_CALL" "Unable to call."
  toStatusCode UnableToSendSMS = E503
  toStatusCode UnableToCall = E503

data ValidationError
  = IncorrectOTP
  | AccessDenied
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError ValidationError where
  toError IncorrectOTP = APIError "INCORRECT_OTP" "Wrong OTP."
  toError AccessDenied = APIError "ACCESS_DENIED" "You have no access to this operation."
  toStatusCode IncorrectOTP = E400
  toStatusCode AccessDenied = E403

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError
  | SQLResultError
  | DBUnknownError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError DatabaseError where
  toError NotPostgresBackend = APIError "DB_NOT_POSTGRES_BACKEND" "Not postgres backend."
  toError SQLRequestError = APIError "DB_SQL_REQUEST_ERROR" "SQL request error."
  toError SQLResultError = APIError "DB_SQL_RESULT_ERROR" "SQL result error."
  toError DBUnknownError = APIError "DB_UNKNOWN_ERROR" "Something unknown happened during work with db."
  toStatusCode NotPostgresBackend = E500
  toStatusCode SQLRequestError = E500
  toStatusCode SQLResultError = E500
  toStatusCode DBUnknownError = E500

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError FCMTokenError where
  toError FCMJSONPathNotConfigured = APIError "FCM_JSON_PATH_NOT_CONFIGURED" "FCM JSON path not configured."
  toError UnableToReadFCMJSONFile = APIError "UNABLE_TO_READ_FCM_JSON_FILE" "Unable to read fcmJson file."
  toStatusCode FCMJSONPathNotConfigured = E500
  toStatusCode UnableToReadFCMJSONFile = E500

data ContextError
  = UnsupportedCoreVer
  | UnsupportedDomainVer
  | InvalidDomain
  | InvalidCountry
  | InvalidCity
  | InvalidAction
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError ContextError where
  toError UnsupportedCoreVer = APIError "UNSUPPORTED_CORE_VERSION" "Unsupported core version."
  toError UnsupportedDomainVer = APIError "UNSUPPORTED_DOMAIN_VERSION" "Unsupported domain version."
  toError InvalidDomain = APIError "INVALID_DOMAIN" "Domain validation failed."
  toError InvalidCountry = APIError "INVALID_COUNTRY" "Country validation failed."
  toError InvalidCity = APIError "INVALID_CITY" "City validation failed."
  toError InvalidAction = APIError "INVALID_ACTION" "Action validation failed."
  toStatusCode UnsupportedCoreVer = E400
  toStatusCode UnsupportedDomainVer = E400
  toStatusCode InvalidDomain = E400
  toStatusCode InvalidCountry = E400
  toStatusCode InvalidCity = E400
  toStatusCode InvalidAction = E400

data GoogleMapsAPIError
  = GMAPIError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError GoogleMapsAPIError where
  toError GMAPIError = APIError "GOOGLE_MAPS_API_ERROR" "Error ocured in google maps API."
  toStatusCode GMAPIError = E500
