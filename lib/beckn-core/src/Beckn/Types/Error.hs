{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import Beckn.TypeClass.IsError
import qualified Beckn.Types.Core.Ack as Ack
import qualified Beckn.Types.Core.Error as Error
import EulerHS.Prelude

data APIError = APIError
  { message :: Ack.Ack,
    error :: Maybe Error.Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

apiError :: Text -> APIError
apiError code = APIError (Ack.Ack "NACK") . Just $ makeError code Nothing

apiErrorWithMsg :: Text -> Text -> APIError
apiErrorWithMsg code msg = APIError (Ack.Ack "NACK") . Just . makeError code $ Just msg

makeError :: Text -> Maybe Text -> Error.Error
makeError err = Error.Error "DOMAIN-ERROR" err Nothing

data AuthError
  = Unauthorized
  | InvalidAuthData
  | TokenExpired
  | InvalidToken
  | AuthBlocked
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AuthError APIError where
  toError Unauthorized = apiErrorWithMsg "UNAUTHORIZED" "Unauthorized action."
  toError InvalidAuthData = apiErrorWithMsg "INVALID_AUTH_DATA" "Authentication data is not valid."
  toError TokenExpired = apiErrorWithMsg "TOKEN_EXPIRED" "Token expired."
  toError InvalidToken = apiErrorWithMsg "INVALID_TOKEN" "Invalid registration token."
  toError AuthBlocked = apiErrorWithMsg "AUTH_BLOCKED" "Authentication process blocked."

data RatingError
  = InvalidRatingValue
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RatingError APIError where
  toError InvalidRatingValue = apiErrorWithMsg "INVALID_RATING_VALUE" "Invalid rating value."

data VehicleError
  = VehicleNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError VehicleError APIError where
  toError VehicleNotFound = apiErrorWithMsg "VEHICLE_NOT_FOUND" "Vehicle not found."

data PersonError
  = PersonNotFound
  | PersonInvalidState
  | PersonOrgExists
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError PersonError APIError where
  toError PersonNotFound = apiErrorWithMsg "PERSON_NOT_FOUND" "Person not found."
  toError PersonInvalidState = apiErrorWithMsg "LOCATION_INVALID_STATE" "Required field is null in this person."
  toError PersonOrgExists = apiErrorWithMsg "ORG_ALREADY_EXISTS" "Person already registered an organization."

data LocationError
  = LocationNotFound
  | LocationInvalidState
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError LocationError APIError where
  toError LocationNotFound = apiErrorWithMsg "LOCATION_NOT_FOUND" "Location not found."
  toError LocationInvalidState = apiErrorWithMsg "LOCATION_INVALID_STATE" "Required field is null in this location."

data RouteError
  = UnableToGetRoute
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RouteError APIError where
  toError UnableToGetRoute = apiErrorWithMsg "UNABLE_TO_GET_ROUTE" "Unable to get route."

data HealthCheckError
  = ServiceUnavailable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError HealthCheckError APIError where
  toError ServiceUnavailable = apiErrorWithMsg "SERVICE_UNAVAILABLE" "Service is down."

data AmbiguousError
  = UnknownError
  | CommonError
  | UnexpectedError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AmbiguousError APIError where
  toError UnknownError = apiErrorWithMsg "UNKNOWN_ERROR" "Something unknown happened."
  toError CommonError = apiErrorWithMsg "COMMON_ERROR" "Something common happened in workflow."
  toError UnexpectedError = apiErrorWithMsg "UNEXPECTED_ERROR" "Happened something, that shouldn't be happen at all."

data OrganizationError
  = OrganizationNotFound
  | OrganizationInvalidState
  | CallbackUrlNotSet
  | CallbackApiKeyNotSet
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError OrganizationError APIError where
  toError OrganizationNotFound = apiErrorWithMsg "ORGANIZATION_NOT_FOUND" "Organization not found."
  toError OrganizationInvalidState = apiErrorWithMsg "ORGANIZATION_INVALID_STATE" "Required field is null in this organization."
  toError CallbackUrlNotSet = apiErrorWithMsg "CALLBACK_URL_NOT_SET" "Callback url for organization is not set."
  toError CallbackApiKeyNotSet = apiErrorWithMsg "CALLBACK_API_KEY_NOT_SET" "Callback api key for organization is not set."

data CaseError
  = CaseNotFound
  | CaseExpired
  | CaseInvalidStatus
  | CaseInvalidState
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CaseError APIError where
  toError CaseNotFound = apiErrorWithMsg "CASE_NOT_FOUND" "Case with this id is not found."
  toError CaseExpired = apiErrorWithMsg "CASE_EXPIRED" "This case expired and no longer valid."
  toError CaseInvalidState = apiErrorWithMsg "CASE_INVALID_STATE" "Required field is null in this case."
  toError CaseInvalidStatus = apiErrorWithMsg "CASE_INVALID_STATUS" "Attempted to do some action in wrong case status."

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceInvalidState
  | ProductInstanceInvalidStatus
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInstanceError APIError where
  toError ProductInstanceNotFound = apiError "PRODUCT_INSTANCE_NOT_FOUND"
  toError ProductInstanceInvalidState = apiErrorWithMsg "PRODUCT_INSTANCE_INVALID_STATE" "Required field is null in this product instance."
  toError ProductInstanceInvalidStatus = apiErrorWithMsg "PRODUCT_INSTANCE_INVALID_STATUS" "Attempted to do some action in wrong product instance status."

data ProductInfoError
  = ProductInfoNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInfoError APIError where
  toError ProductInfoNotFound = apiErrorWithMsg "PRODUCT_INFO_NOT_FOUND" "Product info not found."

data GatewayError
  = GatewaySelectorNotSet
  | NSDLBaseUrlNotSet
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError GatewayError APIError where
  toError GatewaySelectorNotSet = apiErrorWithMsg "GATEWAY_SELECTOR_NOT_SET" "Gateway selector is not set."
  toError NSDLBaseUrlNotSet = apiErrorWithMsg "NSDL_BASEURL_NOT_SET" "NSDL base url is not set."

data ServiceabilityError
  = ProductNotServiceable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ServiceabilityError APIError where
  toError ProductNotServiceable = apiErrorWithMsg "PRODUCT_NOT_SERVICEABLE" "Requested product is not serviceable for some reason."

data APIRequestError
  = InvalidRequest
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError APIRequestError APIError where
  toError InvalidRequest = apiErrorWithMsg "API_REQUEST_ERROR" "Not enough data to complete request."

data CommunicationError
  = UnableToSendSMS
  | UnableToCall
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CommunicationError APIError where
  toError UnableToSendSMS = apiErrorWithMsg "UNABLE_TO_SEND_SMS" "Unable to send SMS."
  toError UnableToCall = apiErrorWithMsg "UNABLE_TO_CALL" "Unable to call."

data ValidationError
  = IncorrectOTP
  | AccessDenied
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ValidationError APIError where
  toError IncorrectOTP = apiErrorWithMsg "INCORRECT_OTP" "Wrong OTP."
  toError AccessDenied = apiErrorWithMsg "ACCESS_DENIED" "You have no access to this operation."

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError
  | SQLResultError
  | DBUnknownError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DatabaseError APIError where
  toError NotPostgresBackend = apiErrorWithMsg "DB_NOT_POSTGRES_BACKEND" "Not postgres backend."
  toError SQLRequestError = apiErrorWithMsg "DB_SQL_REQUEST_ERROR" "SQL request error."
  toError SQLResultError = apiErrorWithMsg "DB_SQL_RESULT_ERROR" "SQL result error."
  toError DBUnknownError = apiErrorWithMsg "DB_UNKNOWN_ERROR" "Something unknown happened during work with db."

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError FCMTokenError APIError where
  toError FCMJSONPathNotConfigured = apiErrorWithMsg "FCM_JSON_PATH_NOT_CONFIGURED" "FCM JSON path not configured."
  toError UnableToReadFCMJSONFile = apiErrorWithMsg "UNABLE_TO_READ_FCM_JSON_FILE" "Unable to read fcmJson file."

data ContextError
  = UnsupportedCoreVer
  | UnsupportedDomainVer
  | InvalidDomain
  | InvalidCountry
  | InvalidCity
  | InvalidAction
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ContextError APIError where
  toError UnsupportedCoreVer = apiErrorWithMsg "UNSUPPORTED_CORE_VERSION" "Unsupported core version."
  toError UnsupportedDomainVer = apiErrorWithMsg "UNSUPPORTED_DOMAIN_VERSION" "Unsupported domain version."
  toError InvalidDomain = apiErrorWithMsg "INVALID_DOMAIN" "Domain validation failed."
  toError InvalidCountry = apiErrorWithMsg "INVALID_COUNTRY" "Country validation failed."
  toError InvalidCity = apiErrorWithMsg "INVALID_CITY" "City validation failed."
  toError InvalidAction = apiErrorWithMsg "INVALID_ACTION" "Action validation failed."

data GoogleMapsAPIError
  = GMAPIError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError GoogleMapsAPIError APIError where
  toError GMAPIError = apiErrorWithMsg "GOOGLE_MAPS_API_ERROR" "Error ocured in google maps API."
