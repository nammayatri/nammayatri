{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import Beckn.TypeClass.IsError
import EulerHS.Prelude
import EulerHS.Types

data Action = ACK | NACK deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

newtype ErrorCode = ErrorCode {fromErrorCode :: Int}
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

newtype ErrorMsg = ErrorMsg {fromErrorMsg :: Text}
  deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

instance IsString ErrorMsg where
  fromString = ErrorMsg . fromString

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

apiError :: Text -> APIError
apiError code = APIError code Nothing

data DomainError
  = AuthErr AuthError
  | QuotaErr QuotaError
  | CommentErr CommentError
  | CustomerErr CustomerError
  | DocumentErr DocumentError
  | HealthCheckErr HealthCheckError
  | PersonErr PersonError
  | RouteErr RouteError
  | ProductInfoErr ProductInfoError
  | LocationErr LocationError
  | TagErr TagError
  | OrganizationErr OrganizationError
  | TransporterErr TransporterError
  | CaseErr CaseError
  | ProductInstanceErr ProductInstanceError
  | ProductErr ProductError
  | UnknownDomainError ErrorMsg
  | DatabaseError DBError
  | SystemErr SystemError
  deriving (Generic, Eq, Show)

data AuthError
  = Unauthorized
  | InvalidAuthData
  | TokenExpired
  | InvalidToken
  | AuthBlocked
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AuthError APIError where
  toError Unauthorized = APIError "UNAUTHORIZED" $ Just "Unauthorized action."
  toError InvalidAuthData = APIError "INVALID_AUTH_DATA" $ Just "Authentication data is not valid."
  toError TokenExpired = APIError "TOKEN_EXPIRED" $ Just "Token expired."
  toError InvalidToken = APIError "INVALID_TOKEN" $ Just "Invalid registration token."
  toError AuthBlocked = APIError "AUTH_BLOCKED" $ Just "Authentication process blocked."

data RatingError
  = InvalidRatingValue
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RatingError APIError where
  toError InvalidRatingValue = APIError "INVALID_RATING_VALUE" $ Just "Invalid rating value."

data VehicleError
  = VehicleNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError VehicleError APIError where
  toError VehicleNotFound = APIError "VEHICLE_NOT_FOUND" $ Just "Vehicle not found."

data QuotaError
  = QuotaNotFound
  | QuotaNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError QuotaError APIError where
  toError QuotaNotFound = apiError "QUOTA_NOT_FOUND"
  toError QuotaNotCreated = apiError "QUOTA_NOT_CREATED"

data CommentError
  = CommentNotFound
  | CommentNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CommentError APIError where
  toError CommentNotFound = apiError "COMMENT_NOT_FOUND"
  toError CommentNotCreated = apiError "COMMENT_NOT_CREATED"

data CustomerError
  = CustomerNotFound
  | CannotCreateCustomer
  | CustomerOrgMismatch
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CustomerError APIError where
  toError CustomerNotFound = apiError "CUSTOMER_NOT_FOUND"
  toError CannotCreateCustomer = apiError "CANNOT_CREATE_CUSTOMER"
  toError CustomerOrgMismatch = apiError "CUSTOMER_ORG_MISMATCH"

data PersonError
  = PersonNotFound
  | PersonInvalidState
  | PersonOrgExists
  | PersonNotUpdated
  | PersonNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError PersonError APIError where
  toError PersonNotFound = APIError "PERSON_NOT_FOUND" $ Just "Person not found."
  toError PersonInvalidState = APIError "LOCATION_INVALID_STATE" $ Just "Required field is null in this person."
  toError PersonOrgExists = APIError "ORG_ALREADY_EXISTS" $ Just "Person already registered an organization."
  toError PersonNotUpdated = apiError "PERSON_NOT_UPDATED"
  toError PersonNotCreated = apiError "PERSON_NOT_CREATED"

data TransporterError
  = TransporterNotFound
  | TransporterNotUpdated
  | TransporterNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TransporterError APIError where
  toError TransporterNotFound = apiError "TRANSPORTER_NOT_FOUND"
  toError TransporterNotUpdated = apiError "TRANSPORTER_NOT_UPDATED"
  toError TransporterNotCreated = apiError "TRANSPORTER_NOT_CREATED"

data LocationError
  = LocationNotFound
  | LocationInvalidState
  | LocationNotUpdated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError LocationError APIError where
  toError LocationNotFound = APIError "LOCATION_NOT_FOUND" $ Just "Location not found."
  toError LocationInvalidState = APIError "LOCATION_INVALID_STATE" $ Just "Required field is null in this location."
  toError LocationNotUpdated = apiError "LOCATION_NOT_UPDATED"

data RouteError
  = UnableToGetRoute
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RouteError APIError where
  toError UnableToGetRoute = APIError "UNABLE_TO_GET_ROUTE" $ Just "Unable to get route."

data DocumentError
  = InvalidPassApplicationId
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DocumentError APIError where
  toError InvalidPassApplicationId = apiError "INVALID_PASS_APPLICATION_ID"

data HealthCheckError
  = ServiceUnavailable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError HealthCheckError APIError where
  toError ServiceUnavailable = APIError "SERVICE_UNAVAILABLE" $ Just "Service is down."

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TagError APIError where
  toError TagNotFound =
    apiError "TAG_NOT_FOUND"

data AmbiguousError
  = UnknownError
  | CommonError
  | UnexpectedError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AmbiguousError APIError where
  toError UnknownError = APIError "UNKNOWN_ERROR" $ Just "Something unknown happened."
  toError CommonError = APIError "COMMON_ERROR" $ Just "Something common happened in workflow."
  toError UnexpectedError = APIError "UNEXPECTED_ERROR" $ Just "Happened something, that shouldn't be happen at all."

data OrganizationError
  = OrganizationNotFound
  | OrganizationInvalidState
  | CallbackUrlNotSet
  | CallbackApiKeyNotSet
  | OrganizationIdMissing
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError OrganizationError APIError where
  toError OrganizationNotFound = APIError "ORGANIZATION_NOT_FOUND" $ Just "Organization not found."
  toError OrganizationInvalidState = APIError "ORGANIZATION_INVALID_STATE" $ Just "Required field is null in this organization."
  toError OrganizationIdMissing = apiError "ORGANIZATION_ID_MISSING"
  toError CallbackUrlNotSet = APIError "CALLBACK_URL_NOT_SET" $ Just "Callback url for organization is not set."
  toError CallbackApiKeyNotSet = APIError "CALLBACK_API_KEY_NOT_SET" $ Just "Callback api key for organization is not set."

data CaseError
  = CaseNotFound
  | CaseNotCreated
  | CaseExpired
  | CaseNotUpdated
  | CaseInvalidState
  | CaseStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CaseError APIError where
  toError CaseNotFound = APIError "CASE_NOT_FOUND" $ Just "Case with this id is not found."
  toError CaseNotCreated = apiError "CASE_NOT_CREATED"
  toError CaseExpired = APIError "CASE_EXPIRED" $ Just "This case expired and no longer valid."
  toError CaseInvalidState = APIError "CASE_INVALID_STATE" $ Just "Required field is null in this case."
  toError CaseNotUpdated = apiError "CASE_NOT_UPDATED"
  toError (CaseStatusTransitionErr msg) = APIError "CASE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceInvalidState
  | ProductInstanceInvalidStatus
  | ProductInstanceStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInstanceError APIError where
  toError ProductInstanceNotFound = apiError "PRODUCT_INSTANCE_NOT_FOUND"
  toError ProductInstanceInvalidState = APIError "PRODUCT_INSTANCE_INVALID_STATE" $ Just "Required field is null in this product instance."
  toError ProductInstanceInvalidStatus = APIError "PRODUCT_INSTANCE_INVALID_STATUS" $ Just "Attempted to do some action in wrong product instance status."
  toError (ProductInstanceStatusTransitionErr msg) =
    APIError "PRODUCT_INSTANCE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductError
  = ProductNotFound
  | ProductInvalidState
  | ProductNotUpdated
  | ProductNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductError APIError where
  toError ProductNotFound = apiError "PRODUCT_NOT_FOUND"
  toError ProductInvalidState = APIError "PRODUCT_INVALID_STATE" $ Just "Required field is null in this product."
  toError ProductNotUpdated = apiError "PRODUCT_NOT_UPDATED"
  toError ProductNotCreated = apiError "PRODUCT_NOT_CREATED"

data ProductInfoError
  = ProductInfoNotFound
  | ProductInfoNotUpdated
  | OtherProductInfoError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInfoError APIError where
  toError ProductInfoNotFound = APIError "PRODUCT_INFO_NOT_FOUND" $ Just "Product info not found."
  toError ProductInfoNotUpdated = apiError "PRODUCT_INFO_NOT_UPDATED"
  toError OtherProductInfoError = apiError "OTHER_PRODUCT_INFO_ERROR"

data GatewayError
  = GatewaySelectorNotSet
  | NSDLBaseUrlNotSet
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError GatewayError APIError where
  toError GatewaySelectorNotSet = APIError "GATEWAY_SELECTOR_NOT_SET" $ Just "Gateway selector is not set."
  toError NSDLBaseUrlNotSet = APIError "NSDL_BASEURL_NOT_SET" $ Just "NSDL base url is not set."

data ServiceabilityError
  = ProductNotServiceable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ServiceabilityError APIError where
  toError ProductNotServiceable = APIError "PRODUCT_NOT_SERVICEABLE" $ Just "Requested product is not serviceable for some reason."

data APIRequestError
  = InvalidRequest
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError APIRequestError APIError where
  toError InvalidRequest = APIError "API_REQUEST_ERROR" $ Just "Not enough data to complete request."

data CommunicationError
  = UnableToSendSMS
  | UnableToCall
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CommunicationError APIError where
  toError UnableToSendSMS = APIError "UNABLE_TO_SEND_SMS" $ Just "Unable to send SMS."
  toError UnableToCall = APIError "UNABLE_TO_CALL" $ Just "Unable to call."

data ValidationError
  = IncorrectOTP
  | AccessDenied
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ValidationError APIError where
  toError IncorrectOTP = APIError "INCORRECT_OTP" $ Just "Wrong OTP."
  toError AccessDenied = APIError "ACCESS_DENIED" $ Just "You have no access to this operation."

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError
  | RecordNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DatabaseError APIError where
  toError NotPostgresBackend = APIError "DB_NOT_POSTGRES_BACKEND" $ Just "Not postgres backend."
  toError SQLRequestError = APIError "DB_SQL_REQUEST_ERROR" $ Just "SQL request error."
  toError RecordNotFound = APIError "DB_RECORD_NOT_FOUND" $ Just "Record not found."

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError FCMTokenError APIError where
  toError FCMJSONPathNotConfigured = APIError "FCM_JSON_PATH_NOT_CONFIGURED" $ Just "FCM JSON path not configured."
  toError UnableToReadFCMJSONFile = APIError "UNABLE_TO_READ_FCM_JSON_FILE" $ Just "Unable to read fcmJson file."

data GoogleMapsAPIError
  = GMAPIError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError GoogleMapsAPIError APIError where
  toError GMAPIError = APIError "GOOGLE_MAPS_API_ERROR" $ Just "Error ocured in google maps API."

newtype SystemError
  = SystemError ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data BecknError = BecknError
  { _errorCode :: ErrorCode,
    _errorMessage :: ErrorMsg,
    _action :: Action
  }
  deriving (Generic, Eq, Show)

instance FromJSON BecknError where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON BecknError where
  toJSON = genericToJSON stripLensPrefixOptions
