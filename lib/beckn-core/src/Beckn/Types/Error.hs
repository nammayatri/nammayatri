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
  { _code :: Text,
    _message :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON APIError where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON APIError where
  toJSON = genericToJSON stripLensPrefixOptions

instance IsError Text APIError where
  toError e = APIError e Nothing

data DomainErrorWithMessage a = DomainErrorWithMessage a Text

--temporary solution until we change all Text Errors to ADT
instance IsError a APIError => IsError (DomainErrorWithMessage a) APIError where
  toError (DomainErrorWithMessage e msg) =
    (toError e)
      { _message = Just msg
      }

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
  toError Unauthorized = APIError "UNAUTHORIZED" Nothing

data QuotaError
  = QuotaNotFound
  | QuotaNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError QuotaError APIError where
  toError QuotaNotFound = APIError "QUOTA_NOT_FOUND" Nothing
  toError QuotaNotCreated = APIError "QUOTA_NOT_CREATED" Nothing

data CommentError
  = CommentNotFound
  | CommentNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CommentError APIError where
  toError CommentNotFound = APIError "COMMENT_NOT_FOUND" Nothing
  toError CommentNotCreated = APIError "COMMENT_NOT_CREATED" Nothing

data CustomerError
  = CustomerNotFound
  | CannotCreateCustomer
  | CustomerOrgMismatch
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CustomerError APIError where
  toError CustomerNotFound = APIError "CUSTOMER_NOT_FOUND" Nothing
  toError CannotCreateCustomer = APIError "CANNOT_CREATE_CUSTOMER" Nothing
  toError CustomerOrgMismatch = APIError "CUSTOMER_ORG_MISMATCH" Nothing

data PersonError
  = PersonNotFound
  | PersonNotUpdated
  | PersonNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError PersonError APIError where
  toError PersonNotFound = APIError "PERSON_NOT_FOUND" Nothing
  toError PersonNotUpdated = APIError "PERSON_NOT_UPDATED" Nothing
  toError PersonNotCreated = APIError "PERSON_NOT_CREATED" Nothing

data TransporterError
  = TransporterNotFound
  | TransporterNotUpdated
  | TransporterNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TransporterError APIError where
  toError TransporterNotFound = APIError "TRANSPORTER_NOT_FOUND" Nothing
  toError TransporterNotUpdated = APIError "TRANSPORTER_NOT_UPDATED" Nothing
  toError TransporterNotCreated = APIError "TRANSPORTER_NOT_CREATED" Nothing

data LocationError
  = LocationNotFound
  | LocationNotUpdated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError LocationError APIError where
  toError LocationNotFound = APIError "LOCATION_NOT_FOUND" Nothing
  toError LocationNotUpdated = APIError "LOCATION_NOT_UPDATED" Nothing

data RouteError
  = RouteNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RouteError APIError where
  toError RouteNotFound = APIError "ROUTE_NOT_FOUND" Nothing

data DocumentError
  = InvalidPassApplicationId
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DocumentError APIError where
  toError InvalidPassApplicationId = APIError "INVALID_PASS_APPLICATION_ID" Nothing

data HealthCheckError
  = HealthCheckNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError HealthCheckError APIError where
  toError HealthCheckNotFound = APIError "HEALTH_CHECK_NOT_FOUND" Nothing

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TagError APIError where
  toError TagNotFound =
    APIError "TAG_NOT_FOUND" Nothing

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
  toError OrganizationNotFound = APIError "ORGANIZATION_NOT_FOUND" Nothing
  toError OrganizationIdMissing = APIError "ORGANIZATION_ID_MISSING" Nothing

data CaseError
  = CaseNotFound
  | CaseNotCreated
  | CaseNotUpdated
  | CaseStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CaseError APIError where
  toError CaseNotFound = APIError "CASE_NOT_FOUND" Nothing
  toError CaseNotCreated = APIError "CASE_NOT_CREATED" Nothing
  toError CaseNotUpdated = APIError "CASE_NOT_UPDATED" Nothing
  toError (CaseStatusTransitionErr msg) = APIError "CASE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInstanceError APIError where
  toError ProductInstanceNotFound = APIError "PRODUCT_INSTANCE_NOT_FOUND" Nothing
  toError (ProductInstanceStatusTransitionErr msg) =
    APIError "PRODUCT_INSTANCE_STATUS_TRANSITION_ERROR" . Just $ fromErrorMsg msg

data ProductError
  = ProductNotFound
  | ProductNotUpdated
  | ProductNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductError APIError where
  toError ProductNotFound = APIError "PRODUCT_NOT_FOUND" Nothing
  toError ProductNotUpdated = APIError "PRODUCT_NOT_UPDATED" Nothing
  toError ProductNotCreated = APIError "PRODUCT_NOT_CREATED" Nothing

data ProductInfoError
  = ProductInfoNotFound
  | ProductInfoNotUpdated
  | OtherProductInfoError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInfoError APIError where
  toError ProductInfoNotFound = APIError "PRODUCT_INFO__NOT_FOUND" Nothing
  toError ProductInfoNotUpdated = APIError "PRODUCT_INFO_NOT_UPDATED" Nothing
  toError OtherProductInfoError = APIError "OTHER_PRODUCT_INFO_ERROR" Nothing

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
