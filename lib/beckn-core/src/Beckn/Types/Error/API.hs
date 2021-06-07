{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.API where

import Beckn.Types.Error.APIError
import Beckn.Types.Error.BecknAPIError
import EulerHS.Prelude
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
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AuthError

instance IsAPIError AuthError where
  toErrorCode = \case
    Unauthorized -> "UNAUTHORIZED"
    InvalidAuthData -> "INVALID_AUTH_DATA"
    TokenExpired -> "TOKEN_EXPIRED"
    TokenNotFound _ -> "TOKEN_NOT_FOUND"
    InvalidToken _ -> "INVALID_TOKEN"
    AuthBlocked _ -> "AUTH_BLOCKED"
    IncorrectOTP -> "INCORRECT_OTP"
    AccessDenied -> "ACCESS_DENIED"
  toMessage = \case
    TokenNotFound tokenId -> Just $ "Token with tokenId \"" <> show tokenId <> "\" not found."
    InvalidToken token -> Just $ "Invalid token: " <> token
    AuthBlocked reason -> Just $ "Authentication process blocked: " <> reason
    AccessDenied -> Just "You have no access to this operation."
    _ -> Nothing
  toHttpCode = \case
    Unauthorized -> E401
    InvalidToken _ -> E401
    AccessDenied -> E403
    _ -> E400

data HeaderError
  = MissingHeader HeaderName
  | InvalidHeader HeaderName Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''HeaderError

instance IsAPIError HeaderError where
  toErrorCode = \case
    MissingHeader _ -> "MISSING_HEADER"
    InvalidHeader _ _ -> "INVALID_HEADER"
  toMessage = \case
    MissingHeader headerName -> Just $ "Header " +|| headerName ||+ " is missing"
    InvalidHeader headerName err -> Just $ "Header " +|| headerName ||+ " is invalid: " +|| err ||+ ""
  toHttpCode _ = E400

data SignatureError
  = SignatureVerificationFailure [Header]
  | CannotDecodeSignature String
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''SignatureError

instance IsAPIError SignatureError where
  toErrorCode = \case
    SignatureVerificationFailure _ -> "SIGNATURE_VERIFICATION_FAILURE"
    CannotDecodeSignature _ -> "CANNOT_DECODE_SIGNATURE"
  toMessage = \case
    CannotDecodeSignature err -> Just (fromString err)
    _ -> Nothing
  toHttpCode _ = E401
  toCustomHeaders (SignatureVerificationFailure headers) = headers
  toCustomHeaders _ = []

data AuthPIError = NotAnExecutor deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AuthPIError

instance IsAPIError AuthPIError where
  toErrorCode NotAnExecutor = "NOT_AN_EXECUTOR"
  toMessage NotAnExecutor = Just "You are not an executor of this ride."
  toHttpCode NotAnExecutor = E403

data VehicleError
  = VehicleNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''VehicleError

instance IsAPIError VehicleError where
  -- TODO: make two different errors (400 and 500) out of this:
  toErrorCode VehicleNotFound = "VEHICLE_NOT_FOUND"
  toHttpCode VehicleNotFound = E400

data PersonError
  = PersonNotFound
  | PersonDoesNotExist
  | PersonFieldNotPresent Text
  | PersonOrgExists
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''PersonError

instance IsAPIError PersonError where
  toErrorCode = \case
    PersonNotFound -> "PERSON_NOT_FOUND"
    PersonDoesNotExist -> "PERSON_DOES_NOT_EXIST"
    PersonFieldNotPresent _ -> "PERSON_FIELD_NOT_PRESENT"
    PersonOrgExists -> "PERSON_ORG_ALREADY_EXISTS"
  toMessage = \case
    PersonFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this person."
    PersonDoesNotExist -> Just "No person matches passed data."
    PersonOrgExists -> Just "Person is already registered in the organization."
    _ -> Nothing
  toHttpCode = \case
    PersonNotFound -> E500
    PersonDoesNotExist -> E400
    PersonFieldNotPresent _ -> E500
    PersonOrgExists -> E400

data LocationError
  = LocationNotFound
  | LocationDoesNotExist
  | LocationFieldNotPresent Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''LocationError

instance IsAPIError LocationError where
  toErrorCode = \case
    LocationNotFound -> "LOCATION_NOT_FOUND"
    LocationDoesNotExist -> "LOCATION_DOES_NOT_EXISTS"
    LocationFieldNotPresent _ -> "LOCATION_FIELD_NOT_PRESENT"
  toMessage = \case
    LocationDoesNotExist -> Just "No location matches passed data."
    LocationFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this location."
    _ -> Nothing
  toHttpCode = \case
    LocationNotFound -> E500
    LocationDoesNotExist -> E400
    LocationFieldNotPresent _ -> E500

data GenericError
  = InternalError Text
  | InvalidRequest Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''GenericError

instance IsAPIError GenericError where
  toErrorCode = \case
    InternalError _ -> "INTERNAL_ERROR"
    InvalidRequest _ -> "INVALID_REQUEST"
  toMessage = \case
    InternalError msg -> Just msg
    InvalidRequest msg -> Just msg
  toHttpCode = \case
    InternalError _ -> E500
    InvalidRequest _ -> E400

data OrganizationError
  = OrgNotFound
  | OrgDoesNotExist
  | OrgFieldNotPresent Text
  | OrgMobilePhoneUsed
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''OrganizationError

instance IsAPIError OrganizationError where
  toErrorCode = \case
    OrgNotFound -> "ORGANIZATION_NOT_FOUND"
    OrgDoesNotExist -> "ORGANIZATION_DOES_NOT_EXISTS"
    OrgFieldNotPresent _ -> "ORGANIZATION_FIELD_NOT_PRESENT"
    OrgMobilePhoneUsed -> "ORGANIZATION_MOBILE_PHONE_USED"
  toMessage = \case
    OrgDoesNotExist -> Just "No organization matches passed data."
    OrgFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this organization."
    OrgMobilePhoneUsed -> Just "Mobile phone already used by another organization."
    _ -> Nothing
  toHttpCode OrgDoesNotExist = E400
  toHttpCode OrgMobilePhoneUsed = E400
  toHttpCode _ = E500

data CaseError
  = CaseNotFound
  | CaseDoesNotExist
  | CaseExpired
  | CaseInvalidStatus Text
  | CaseFieldNotPresent Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''CaseError

instance IsAPIError CaseError where
  toErrorCode = \case
    CaseNotFound -> "CASE_NOT_FOUND"
    CaseDoesNotExist -> "CASE_DOES_NOT_EXISTS"
    CaseExpired -> "CASE_EXPIRED"
    CaseFieldNotPresent _ -> "CASE_FIELD_NOT_PRESENT"
    CaseInvalidStatus _ -> "CASE_INVALID_STATUS"
  toMessage = \case
    CaseDoesNotExist -> Just "No case matches passed data."
    CaseFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this case."
    CaseInvalidStatus msg -> Just $ "Attempted to do some action in wrong case status. " <> msg
    _ -> Nothing
  toHttpCode = \case
    CaseNotFound -> E500
    CaseDoesNotExist -> E400
    CaseExpired -> E400
    CaseFieldNotPresent _ -> E500
    CaseInvalidStatus _ -> E400

data ProductInstanceError
  = PINotFound
  | PIDoesNotExist
  | PIFieldNotPresent Text
  | PIInvalidStatus Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ProductInstanceError

instance IsAPIError ProductInstanceError where
  toErrorCode = \case
    PINotFound -> "PI_NOT_FOUND"
    PIDoesNotExist -> "PI_DOES_NOT_EXISTS"
    PIFieldNotPresent _ -> "PI_FIELD_NOT_PRESENT"
    PIInvalidStatus _ -> "PI_INVALID_STATUS"
  toMessage = \case
    PIDoesNotExist -> Just "No product instance matches passed data."
    PIFieldNotPresent field -> Just $ "Required field " <> field <> " is null for this product instance."
    PIInvalidStatus msg -> Just $ "Attempted to do some action in wrong PI status. " <> msg
    _ -> Nothing
  toHttpCode = \case
    PINotFound -> E500
    PIDoesNotExist -> E400
    PIFieldNotPresent _ -> E500
    PIInvalidStatus _ -> E400

data GatewayError
  = GatewaySelectorNotSet
  | NSDLBaseUrlNotSet
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''GatewayError

instance IsAPIError GatewayError where
  toErrorCode GatewaySelectorNotSet = "GATEWAY_SELECTOR_NOT_SET"
  toErrorCode NSDLBaseUrlNotSet = "NSDL_BASEURL_NOT_SET"

data DatabaseError
  = NotPostgresBackend
  | SQLRequestError Text Text
  | SQLResultError Text
  | DBUnknownError Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''DatabaseError

instance IsAPIError DatabaseError where
  toErrorCode = \case
    NotPostgresBackend -> "DB_NOT_POSTGRES_BACKEND"
    SQLRequestError _ _ -> "DB_SQL_REQUEST_ERROR"
    SQLResultError _ -> "DB_SQL_RESULT_ERROR"
    DBUnknownError _ -> "DB_UNKNOWN_ERROR"
  toMessage = \case
    SQLRequestError sqlErr desc -> Just $ "SQL request error: " <> sqlErr <> ". Description: " <> desc
    SQLResultError msg -> Just msg
    DBUnknownError msg -> Just msg
    _ -> Nothing
  toHttpCode _ = E500

data FCMTokenError
  = FCMJSONPathNotConfigured
  | UnableToReadFCMJSONFile
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''FCMTokenError

instance IsAPIError FCMTokenError where
  toErrorCode FCMJSONPathNotConfigured = "FCM_JSON_PATH_NOT_CONFIGURED"
  toErrorCode UnableToReadFCMJSONFile = "UNABLE_TO_READ_FCM_JSON_FILE"
  toHttpCode _ = E500

data ContextError
  = UnsupportedCoreVer
  | UnsupportedDomainVer
  | InvalidDomain
  | InvalidCountry
  | InvalidCity
  | InvalidAction
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ContextError

instance IsAPIError ContextError where
  toErrorCode UnsupportedCoreVer = "UNSUPPORTED_CORE_VERSION"
  toErrorCode UnsupportedDomainVer = "UNSUPPORTED_DOMAIN_VERSION"
  toErrorCode InvalidDomain = "INVALID_DOMAIN"
  toErrorCode InvalidCountry = "INVALID_COUNTRY"
  toErrorCode InvalidCity = "INVALID_CITY"
  toErrorCode InvalidAction = "INVALID_ACTION"
  toHttpCode _ = E400

instance IsBecknAPIError ContextError where
  toType _ = CONTEXT_ERROR

data ExternalAPICallError
  = ExternalAPICallError BaseUrl ClientError
  | ExternalAPICallErrorWithCode Text BaseUrl ClientError
  | ExternalAPIResponseError Text Error
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ExternalAPICallError

instance IsAPIError ExternalAPICallError where
  toErrorCode = \case
    ExternalAPICallError _ _ -> "EXTERNAL_API_CALL_ERROR"
    ExternalAPICallErrorWithCode code _ _ -> code
    ExternalAPIResponseError _ _ -> "EXTERNAL_API_RESPONSE_ERROR"
  toMessage = \case
    ExternalAPICallError url err -> externalAPICallErrorMessage url err
    ExternalAPICallErrorWithCode _ url err -> externalAPICallErrorMessage url err
    ExternalAPIResponseError ep err ->
      Just $
        "Beckn " <> ep <> " request returned error code " <> code err
          <> maybe "" ("with message: " <>) (message err)

externalAPICallErrorMessage :: BaseUrl -> ClientError -> Maybe Text
externalAPICallErrorMessage baseUrl clientErr =
  Just $
    "Failure in the external API call to "
      <> toText (showBaseUrl baseUrl)
      <> ": "
      <> show clientErr

data HealthCheckError
  = ServiceUnavailable
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''HealthCheckError

instance IsAPIError HealthCheckError where
  toErrorCode ServiceUnavailable = "SERVICE_UNAVAILABLE"
  toHttpCode ServiceUnavailable = E503

data RouteError
  = RouteRequestError BaseUrl ClientError
  | RouteNotLatLong
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''RouteError

instance IsAPIError RouteError where
  toErrorCode = \case
    RouteRequestError _ _ -> "UNABLE_TO_GET_ROUTE"
    RouteNotLatLong -> "GET_ROUTE_UNSUPPORTED_FORMAT"
  toMessage = \case
    RouteRequestError url err -> externalAPICallErrorMessage url err
    RouteNotLatLong -> Just "Not supporting waypoints other than LatLong."

data ServerError
  = ServerUnavailable
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ServerError

instance IsAPIError ServerError where
  toErrorCode ServerUnavailable = "SERVER_UNAVAILABLE"
  toMessage ServerUnavailable = Just "Server is working, but is not available."
  toHttpCode ServerUnavailable = E503

newtype RedisError
  = RedisError Text
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''RedisError

instance IsAPIError RedisError where
  toErrorCode = \case
    RedisError _ -> "REDIS_ERROR"
  toMessage = \case
    RedisError err -> Just $ "Error: " <> err
  toHttpCode _ = E500
