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
  toError e =
    APIError
      { _code = e,
        _message = Nothing
      }

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
  = UnAuthorized
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AuthError APIError where
  toError _ =
    APIError
      { _code = "UNAUTHORIZED",
        _message = Nothing
      }

data QuotaError
  = QuotaNotFound
  | QuotaNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError QuotaError APIError where
  toError QuotaNotFound =
    APIError
      { _code = "QUOTA_NOT_FOUND",
        _message = Nothing
      }
  toError QuotaNotCreated =
    APIError
      { _code = "QUOTA_NOT_CREATED",
        _message = Nothing
      }

data CommentError
  = CommentNotFound
  | CommentNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CommentError APIError where
  toError CommentNotFound =
    APIError
      { _code = "COMMENT_NOT_FOUND",
        _message = Nothing
      }
  toError CommentNotCreated =
    APIError
      { _code = "COMMENT_NOT_CREATED",
        _message = Nothing
      }

data CustomerError
  = CustomerNotFound
  | CannotCreateCustomer
  | CustomerOrgMismatch
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CustomerError APIError where
  toError CustomerNotFound =
    APIError
      { _code = "CUSTOMER_NOT_FOUND",
        _message = Nothing
      }
  toError CannotCreateCustomer =
    APIError
      { _code = "CANNOT_CREATE_CUSTOMER",
        _message = Nothing
      }
  toError CustomerOrgMismatch =
    APIError
      { _code = "CUSTOMER_ORG_MISMATCH",
        _message = Nothing
      }

data PersonError
  = PersonNotFound
  | PersonNotUpdated
  | PersonNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError PersonError APIError where
  toError PersonNotFound =
    APIError
      { _code = "PERSON_NOT_FOUND",
        _message = Nothing
      }
  toError PersonNotUpdated =
    APIError
      { _code = "PERSON_NOT_UPDATED",
        _message = Nothing
      }
  toError PersonNotCreated =
    APIError
      { _code = "PERSON_NOT_CREATED",
        _message = Nothing
      }

data TransporterError
  = TransporterNotFound
  | TransporterNotUpdated
  | TransporterNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TransporterError APIError where
  toError TransporterNotFound =
    APIError
      { _code = "TRANSPORTER_NOT_FOUND",
        _message = Nothing
      }
  toError TransporterNotUpdated =
    APIError
      { _code = "TRANSPORTER_NOT_UPDATED",
        _message = Nothing
      }
  toError TransporterNotCreated =
    APIError
      { _code = "TRANSPORTER_NOT_CREATED",
        _message = Nothing
      }

data LocationError
  = LocationNotFound
  | LocationNotUpdated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError LocationError APIError where
  toError LocationNotFound =
    APIError
      { _code = "LOCATION_NOT_FOUND",
        _message = Nothing
      }
  toError LocationNotUpdated =
    APIError
      { _code = "LOCATION_NOT_UPDATED",
        _message = Nothing
      }

data RouteError
  = RouteNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError RouteError APIError where
  toError RouteNotFound =
    APIError
      { _code = "ROUTE_NOT_FOUND",
        _message = Nothing
      }

data DocumentError
  = InvalidPassApplicationId
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DocumentError APIError where
  toError InvalidPassApplicationId =
    APIError
      { _code = "INVALID_PASS_APPLICATION_ID",
        _message = Nothing
      }

data HealthCheckError
  = HealthCheckNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError HealthCheckError APIError where
  toError HealthCheckNotFound =
    APIError
      { _code = "HEALTH_CHECK_NOT_FOUND",
        _message = Nothing
      }

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError TagError APIError where
  toError TagNotFound =
    APIError
      { _code = "TAG_NOT_FOUND",
        _message = Nothing
      }

newtype UnknownError
  = UnknownError ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError UnknownError APIError where
  toError (UnknownError msg) =
    APIError
      { _code = "UNKNOWN_ERROR",
        _message = Just $ fromErrorMsg msg
      }

data OrganizationError
  = OrganizationNotFound
  | OrganizationIdMissing
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError OrganizationError APIError where
  toError OrganizationNotFound =
    APIError
      { _code = "ORGANIZATION_NOT_FOUND",
        _message = Nothing
      }
  toError OrganizationIdMissing =
    APIError
      { _code = "ORGANIZATION_ID_MISSING",
        _message = Nothing
      }

data CaseError
  = CaseNotFound
  | CaseNotCreated
  | CaseNotUpdated
  | CaseStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError CaseError APIError where
  toError CaseNotFound =
    APIError
      { _code = "CASE_NOT_FOUND",
        _message = Nothing
      }
  toError CaseNotCreated =
    APIError
      { _code = "CASE_NOT_CREATED",
        _message = Nothing
      }
  toError CaseNotUpdated =
    APIError
      { _code = "CASE_NOT_UPDATED",
        _message = Nothing
      }
  toError (CaseStatusTransitionErr msg) =
    APIError
      { _code = "CASE_STATUS_TRANSITION_ERROR",
        _message = Just $ fromErrorMsg msg
      }

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInstanceError APIError where
  toError ProductInstanceNotFound =
    APIError
      { _code = "PRODUCT_INSTANCE_NOT_FOUND",
        _message = Nothing
      }
  toError (ProductInstanceStatusTransitionErr msg) =
    APIError
      { _code = "PRODUCT_INSTANCE_STATUS_TRANSITION_ERROR",
        _message = Just $ fromErrorMsg msg
      }

data ProductError
  = ProductNotFound
  | ProductNotUpdated
  | ProductNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductError APIError where
  toError ProductNotFound =
    APIError
      { _code = "PRODUCT_NOT_FOUND",
        _message = Nothing
      }
  toError ProductNotUpdated =
    APIError
      { _code = "PRODUCT_NOT_UPDATED",
        _message = Nothing
      }
  toError ProductNotCreated =
    APIError
      { _code = "PRODUCT_NOT_CREATED",
        _message = Nothing
      }

data ProductInfoError
  = ProductInfoNotFound
  | ProductInfoNotUpdated
  | OtherProductInfoError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError ProductInfoError APIError where
  toError ProductInfoNotFound =
    APIError
      { _code = "PRODUCT_INFO__NOT_FOUND",
        _message = Nothing
      }
  toError ProductInfoNotUpdated =
    APIError
      { _code = "PRODUCT_INFO_NOT_UPDATED",
        _message = Nothing
      }
  toError OtherProductInfoError =
    APIError
      { _code = "OTHER_PRODUCT_INFO_ERROR",
        _message = Nothing
      }

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
