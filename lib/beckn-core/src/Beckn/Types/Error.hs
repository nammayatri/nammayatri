{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError.APIError
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import EulerHS.Prelude
import EulerHS.Types (KVDBReply)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (HeaderName)
import Servant.Client (BaseUrl, ClientError, showBaseUrl)

-- TODO: sort out proper codes, namings and usages for Unauthorized and AccessDenied
data AuthError
  = Unauthorized
  | InvalidAuthData
  | TokenExpired
  | TokenNotFound Text
  | InvalidToken Text
  | AuthBlocked Text
  | IncorrectOTP
  | AccessDenied
  | HitsLimitError Int
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AuthError

instance IsBaseError AuthError where
  toMessage = \case
    TokenNotFound tokenId -> Just $ "Token with tokenId \"" <> show tokenId <> "\" not found."
    InvalidToken token -> Just $ "Invalid token: " <> token
    AuthBlocked reason -> Just $ "Authentication process blocked: " <> reason
    AccessDenied -> Just "You have no access to this operation."
    HitsLimitError hitsLimitResetTime -> Just $ "Hits limit reached. Try again in " <> show hitsLimitResetTime <> " sec."
    _ -> Nothing

instance IsHTTPError AuthError where
  toErrorCode = \case
    Unauthorized -> "UNAUTHORIZED"
    InvalidAuthData -> "INVALID_AUTH_DATA"
    TokenExpired -> "TOKEN_EXPIRED"
    TokenNotFound _ -> "TOKEN_NOT_FOUND"
    InvalidToken _ -> "INVALID_TOKEN"
    AuthBlocked _ -> "AUTH_BLOCKED"
    IncorrectOTP -> "INCORRECT_OTP"
    AccessDenied -> "ACCESS_DENIED"
    HitsLimitError _ -> "HITS_LIMIT_EXCEED"
  toHttpCode = \case
    Unauthorized -> E401
    InvalidToken _ -> E401
    AccessDenied -> E403
    HitsLimitError _ -> E429
    _ -> E400

instance IsAPIError AuthError

data HeaderError
  = MissingHeader HeaderName
  | InvalidHeader HeaderName Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''HeaderError

instance IsBaseError HeaderError where
  toMessage = \case
    MissingHeader headerName -> Just $ "Header " +|| headerName ||+ " is missing"
    InvalidHeader headerName err -> Just $ "Header " +|| headerName ||+ " is invalid: " +|| err ||+ ""

instance IsHTTPError HeaderError where
  toErrorCode = \case
    MissingHeader _ -> "MISSING_HEADER"
    InvalidHeader _ _ -> "INVALID_HEADER"
  toHttpCode _ = E400

instance IsAPIError HeaderError

data SignatureError
  = SignatureVerificationFailure [Header]
  | CannotDecodeSignature String
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''SignatureError

instance IsBaseError SignatureError where
  toMessage = \case
    CannotDecodeSignature err -> Just (fromString err)
    _ -> Nothing

instance IsHTTPError SignatureError where
  toErrorCode = \case
    SignatureVerificationFailure _ -> "SIGNATURE_VERIFICATION_FAILURE"
    CannotDecodeSignature _ -> "CANNOT_DECODE_SIGNATURE"
  toHttpCode _ = E401
  toCustomHeaders (SignatureVerificationFailure headers) = headers
  toCustomHeaders _ = []

instance IsAPIError SignatureError

data AuthPIError = NotAnExecutor deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AuthPIError

instance IsBaseError AuthPIError where
  toMessage NotAnExecutor = Just "You are not an executor of this ride."

instance IsHTTPError AuthPIError where
  toErrorCode NotAnExecutor = "NOT_AN_EXECUTOR"
  toHttpCode NotAnExecutor = E403

instance IsAPIError AuthPIError

data VehicleError
  = VehicleNotFound
  | VehicleDoesNotExist
  | VehicleAlreadyLinked
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''VehicleError

instance IsBaseError VehicleError

instance IsHTTPError VehicleError where
  toErrorCode = \case
    VehicleNotFound -> "VEHICLE_NOT_FOUND"
    VehicleDoesNotExist -> "VEHICLE_DOES_NOT_EXIST"
    VehicleAlreadyLinked -> "VEHICLE_ALREADY_LINKED"
  toHttpCode = \case
    VehicleNotFound -> E500
    VehicleDoesNotExist -> E400
    VehicleAlreadyLinked -> E400

instance IsAPIError VehicleError

data PersonError
  = PersonNotFound
  | PersonDoesNotExist
  | PersonFieldNotPresent Text
  | PersonOrgExists
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''PersonError

instance IsBaseError PersonError where
  toMessage = \case
    PersonFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this person."
    PersonDoesNotExist -> Just "No person matches passed data."
    PersonOrgExists -> Just "Person is already registered in the organization."
    _ -> Nothing

instance IsHTTPError PersonError where
  toErrorCode = \case
    PersonNotFound -> "PERSON_NOT_FOUND"
    PersonDoesNotExist -> "PERSON_DOES_NOT_EXIST"
    PersonFieldNotPresent _ -> "PERSON_FIELD_NOT_PRESENT"
    PersonOrgExists -> "PERSON_ORG_ALREADY_EXISTS"
  toHttpCode = \case
    PersonNotFound -> E500
    PersonDoesNotExist -> E400
    PersonFieldNotPresent _ -> E500
    PersonOrgExists -> E400

instance IsAPIError PersonError

data LocationError
  = LocationNotFound
  | LocationDoesNotExist
  | LocationFieldNotPresent Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''LocationError

instance IsBaseError LocationError where
  toMessage = \case
    LocationDoesNotExist -> Just "No location matches passed data."
    LocationFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this location."
    _ -> Nothing

instance IsHTTPError LocationError where
  toErrorCode = \case
    LocationNotFound -> "LOCATION_NOT_FOUND"
    LocationDoesNotExist -> "LOCATION_DOES_NOT_EXISTS"
    LocationFieldNotPresent _ -> "LOCATION_FIELD_NOT_PRESENT"
  toHttpCode = \case
    LocationNotFound -> E500
    LocationDoesNotExist -> E400
    LocationFieldNotPresent _ -> E500

instance IsAPIError LocationError

data GenericError
  = InternalError Text
  | InvalidRequest Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''GenericError

instance IsBaseError GenericError where
  toMessage = \case
    InternalError msg -> Just msg
    InvalidRequest msg -> Just msg

instance IsHTTPError GenericError where
  toErrorCode = \case
    InternalError _ -> "INTERNAL_ERROR"
    InvalidRequest _ -> "INVALID_REQUEST"
  toHttpCode = \case
    InternalError _ -> E500
    InvalidRequest _ -> E400

instance IsAPIError GenericError

data OrganizationError
  = OrgNotFound
  | OrgDoesNotExist
  | OrgFieldNotPresent Text
  | OrgMobilePhoneUsed
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''OrganizationError

instance IsBaseError OrganizationError where
  toMessage = \case
    OrgDoesNotExist -> Just "No organization matches passed data."
    OrgFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this organization."
    OrgMobilePhoneUsed -> Just "Mobile phone already used by another organization."
    _ -> Nothing

instance IsHTTPError OrganizationError where
  toErrorCode = \case
    OrgNotFound -> "ORGANIZATION_NOT_FOUND"
    OrgDoesNotExist -> "ORGANIZATION_DOES_NOT_EXISTS"
    OrgFieldNotPresent _ -> "ORGANIZATION_FIELD_NOT_PRESENT"
    OrgMobilePhoneUsed -> "ORGANIZATION_MOBILE_PHONE_USED"
  toHttpCode OrgDoesNotExist = E400
  toHttpCode OrgMobilePhoneUsed = E400
  toHttpCode _ = E500

instance IsAPIError OrganizationError

data CaseError
  = CaseNotFound
  | CaseDoesNotExist
  | CaseExpired
  | CaseInvalidStatus Text
  | CaseFieldNotPresent Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''CaseError

instance IsBaseError CaseError where
  toMessage = \case
    CaseDoesNotExist -> Just "No case matches passed data."
    CaseFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this case."
    CaseInvalidStatus msg -> Just $ "Attempted to do some action in wrong case status. " <> msg
    _ -> Nothing

instance IsHTTPError CaseError where
  toErrorCode = \case
    CaseNotFound -> "CASE_NOT_FOUND"
    CaseDoesNotExist -> "CASE_DOES_NOT_EXISTS"
    CaseExpired -> "CASE_EXPIRED"
    CaseFieldNotPresent _ -> "CASE_FIELD_NOT_PRESENT"
    CaseInvalidStatus _ -> "CASE_INVALID_STATUS"
  toHttpCode = \case
    CaseNotFound -> E500
    CaseDoesNotExist -> E400
    CaseExpired -> E400
    CaseFieldNotPresent _ -> E500
    CaseInvalidStatus _ -> E400

instance IsAPIError CaseError

data ProductInstanceError
  = PINotFound
  | PIDoesNotExist
  | PIFieldNotPresent Text
  | PIInvalidStatus Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ProductInstanceError

instance IsBaseError ProductInstanceError where
  toMessage = \case
    PIDoesNotExist -> Just "No product instance matches passed data."
    PIFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this product instance."
    PIInvalidStatus msg -> Just $ "Attempted to do some action in wrong PI status. " <> msg
    _ -> Nothing

instance IsHTTPError ProductInstanceError where
  toErrorCode = \case
    PINotFound -> "PI_NOT_FOUND"
    PIDoesNotExist -> "PI_DOES_NOT_EXISTS"
    PIFieldNotPresent _ -> "PI_FIELD_NOT_PRESENT"
    PIInvalidStatus _ -> "PI_INVALID_STATUS"
  toHttpCode = \case
    PINotFound -> E500
    PIDoesNotExist -> E400
    PIFieldNotPresent _ -> E500
    PIInvalidStatus _ -> E400

instance IsAPIError ProductInstanceError

data GatewayError
  = GatewaySelectorNotSet
  | NSDLBaseUrlNotSet
  | UnsupportedGatewaySelector
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''GatewayError

instance IsBaseError GatewayError

instance IsHTTPError GatewayError where
  toErrorCode GatewaySelectorNotSet = "GATEWAY_SELECTOR_NOT_SET"
  toErrorCode NSDLBaseUrlNotSet = "NSDL_BASEURL_NOT_SET"
  toErrorCode UnsupportedGatewaySelector = "UNSUPPORTED_GATEWAY_SELECTOR"

instance IsAPIError GatewayError

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError Text Text
  | SQLResultError Text
  | DBUnknownError Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''DatabaseError

instance IsBaseError DatabaseError where
  toMessage = \case
    SQLRequestError sqlErr desc -> Just $ "SQL request error: " <> sqlErr <> ". Description: " <> desc
    SQLResultError msg -> Just msg
    DBUnknownError msg -> Just msg
    _ -> Nothing

instance IsHTTPError DatabaseError where
  toErrorCode = \case
    NotPostgresBackend -> "DB_NOT_POSTGRES_BACKEND"
    SQLRequestError _ _ -> "DB_SQL_REQUEST_ERROR"
    SQLResultError _ -> "DB_SQL_RESULT_ERROR"
    DBUnknownError _ -> "DB_UNKNOWN_ERROR"
  toHttpCode _ = E500

instance IsAPIError DatabaseError

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''FCMTokenError

instance IsBaseError FCMTokenError

instance IsHTTPError FCMTokenError where
  toErrorCode FCMJSONPathNotConfigured = "FCM_JSON_PATH_NOT_CONFIGURED"
  toErrorCode UnableToReadFCMJSONFile = "UNABLE_TO_READ_FCM_JSON_FILE"
  toHttpCode _ = E500

instance IsAPIError FCMTokenError

data ContextError
  = UnsupportedCoreVer
  | UnsupportedDomainVer
  | InvalidDomain
  | InvalidCountry
  | InvalidCity
  | InvalidAction
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ContextError

instance IsBaseError ContextError

instance IsHTTPError ContextError where
  toErrorCode UnsupportedCoreVer = "UNSUPPORTED_CORE_VERSION"
  toErrorCode UnsupportedDomainVer = "UNSUPPORTED_DOMAIN_VERSION"
  toErrorCode InvalidDomain = "INVALID_DOMAIN"
  toErrorCode InvalidCountry = "INVALID_COUNTRY"
  toErrorCode InvalidCity = "INVALID_CITY"
  toErrorCode InvalidAction = "INVALID_ACTION"
  toHttpCode _ = E400

instance IsAPIError ContextError

instance IsBecknAPIError ContextError where
  toType _ = CONTEXT_ERROR

data ExternalAPICallError
  = ExternalAPICallError (Maybe Text) BaseUrl ClientError
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ExternalAPICallError

instance IsBaseError ExternalAPICallError where
  toMessage (ExternalAPICallError _ url err) = externalAPICallErrorMessage url err

instance IsHTTPError ExternalAPICallError where
  toErrorCode (ExternalAPICallError codeMb _ _) = fromMaybe "EXTERNAL_API_CALL_ERROR" codeMb

instance IsAPIError ExternalAPICallError

externalAPICallErrorMessage :: BaseUrl -> ClientError -> Maybe Text
externalAPICallErrorMessage baseUrl clientErr =
  Just $
    "Failure in the external API call to "
      <> toText (showBaseUrl baseUrl)
      <> ": "
      <> show clientErr

newtype EmailSendingError
  = EmailSendingError Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''EmailSendingError

instance IsBaseError EmailSendingError where
  toMessage (EmailSendingError msg) = Just msg

instance IsHTTPError EmailSendingError where
  toErrorCode (EmailSendingError _) = "EMAIL_SENDING_ERROR"

instance IsAPIError EmailSendingError

data HealthCheckError
  = ServiceUnavailable
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''HealthCheckError

instance IsBaseError HealthCheckError

instance IsHTTPError HealthCheckError where
  toErrorCode ServiceUnavailable = "SERVICE_UNAVAILABLE"
  toHttpCode ServiceUnavailable = E503

instance IsAPIError HealthCheckError

data RouteError
  = RouteRequestError BaseUrl ClientError
  | RouteNotLatLong
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''RouteError

instance IsBaseError RouteError where
  toMessage = \case
    RouteRequestError url err -> externalAPICallErrorMessage url err
    RouteNotLatLong -> Just "Not supporting waypoints other than LatLong."

instance IsHTTPError RouteError where
  toErrorCode = \case
    RouteRequestError _ _ -> "UNABLE_TO_GET_ROUTE"
    RouteNotLatLong -> "GET_ROUTE_UNSUPPORTED_FORMAT"

instance IsAPIError RouteError

data ServerError
  = ServerUnavailable
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ServerError

instance IsBaseError ServerError where
  toMessage ServerUnavailable = Just "Server is working, but is not available."

instance IsHTTPError ServerError where
  toErrorCode ServerUnavailable = "SERVER_UNAVAILABLE"
  toHttpCode ServerUnavailable = E503

instance IsAPIError ServerError

newtype RedisError
  = RedisError KVDBReply
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''RedisError

instance IsBaseError RedisError where
  toMessage = \case
    RedisError err -> Just $ show err

instance IsHTTPError RedisError where
  toErrorCode = \case
    RedisError _ -> "REDIS_ERROR"
  toHttpCode _ = E500

instance IsAPIError RedisError

newtype ActionNotSupported = ActionNotSupported Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ActionNotSupported

instance IsBaseError ActionNotSupported where
  toMessage (ActionNotSupported action) = Just $ "Action " <> action <> " is not supported"

instance IsHTTPError ActionNotSupported where
  toErrorCode _ = "ACTION_NOT_SUPPORTED"
  toHttpCode _ = E400

instance IsAPIError ActionNotSupported

newtype SMSError = SMSError Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''SMSError

instance IsBaseError SMSError where
  toMessage = \case
    SMSError err -> Just err

instance IsHTTPError SMSError where
  toErrorCode = \case
    SMSError _ -> "SMS_NOT_SENT"

instance IsAPIError SMSError
