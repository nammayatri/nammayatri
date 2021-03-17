{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import Beckn.TypeClass.IsError
import EulerHS.Prelude
import EulerHS.Types (DBError)

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

instance IsError Text APIError where
  toError e = APIError e Nothing

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
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AuthError APIError where
  toError Unauthorized = apiError "UNAUTHORIZED"

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
  | PersonNotUpdated
  | PersonNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError PersonError APIError where
  toError PersonNotFound = apiError "PERSON_NOT_FOUND"
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
  | LocationNotUpdated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError LocationError APIError where
  toError LocationNotFound = apiError "LOCATION_NOT_FOUND"
  toError LocationNotUpdated = apiError "LOCATION_NOT_UPDATED"

data RouteError
  = RouteNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RouteError APIError where
  toError RouteNotFound = apiError "ROUTE_NOT_FOUND"

data DocumentError
  = InvalidPassApplicationId
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DocumentError APIError where
  toError InvalidPassApplicationId = apiError "INVALID_PASS_APPLICATION_ID"

data HealthCheckError
  = HealthCheckNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError HealthCheckError APIError where
  toError HealthCheckNotFound = apiError "HEALTH_CHECK_NOT_FOUND"

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TagError APIError where
  toError TagNotFound =
    apiError "TAG_NOT_FOUND"

newtype UnknownError
  = UnknownError ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError UnknownError APIError where
  toError (UnknownError msg) = APIError "UNKNOWN_ERROR" . Just $ fromErrorMsg msg

data OrganizationError
  = OrganizationNotFound
  | OrganizationIdMissing
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError OrganizationError APIError where
  toError OrganizationNotFound = apiError "ORGANIZATION_NOT_FOUND"
  toError OrganizationIdMissing = apiError "ORGANIZATION_ID_MISSING"

data CaseError
  = CaseNotFound
  | CaseNotCreated
  | CaseNotUpdated
  | CaseStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CaseError APIError where
  toError CaseNotFound = apiError "CASE_NOT_FOUND"
  toError CaseNotCreated = apiError "CASE_NOT_CREATED"
  toError CaseNotUpdated = apiError "CASE_NOT_UPDATED"
  toError (CaseStatusTransitionErr msg) = APIError "CASE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInstanceError APIError where
  toError ProductInstanceNotFound = apiError "PRODUCT_INSTANCE_NOT_FOUND"
  toError (ProductInstanceStatusTransitionErr msg) =
    APIError "PRODUCT_INSTANCE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductError
  = ProductNotFound
  | ProductNotUpdated
  | ProductNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductError APIError where
  toError ProductNotFound = apiError "PRODUCT_NOT_FOUND"
  toError ProductNotUpdated = apiError "PRODUCT_NOT_UPDATED"
  toError ProductNotCreated = apiError "PRODUCT_NOT_CREATED"

data ProductInfoError
  = ProductInfoNotFound
  | ProductInfoNotUpdated
  | OtherProductInfoError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInfoError APIError where
  toError ProductInfoNotFound = apiError "PRODUCT_INFO__NOT_FOUND"
  toError ProductInfoNotUpdated = apiError "PRODUCT_INFO_NOT_UPDATED"
  toError OtherProductInfoError = apiError "OTHER_PRODUCT_INFO_ERROR"

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
