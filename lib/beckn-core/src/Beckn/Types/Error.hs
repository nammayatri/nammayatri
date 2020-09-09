{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import EulerHS.Prelude
import EulerHS.Types (DBError)

data Action = ACK | NACK deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

newtype ErrorCode = ErrorCode {fromErrorCode :: Int}
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

newtype ErrorMsg = ErrorMsg {fromErrorMsg :: Text}
  deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

instance IsString ErrorMsg where
  fromString = ErrorMsg . fromString

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
  | OrganisationErr OrganisationError
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

data QuotaError
  = QuotaNotFound
  | QuotaNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data CommentError
  = CommentNotFound
  | CommentNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data CustomerError
  = CustomerNotFound
  | CannotCreateCustomer
  | CustomerOrgMismatch
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data PersonError
  = PersonNotFound
  | PersonNotUpdated
  | PersonNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data TransporterError
  = TransporterNotFound
  | TransporterNotUpdated
  | TransporterNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data LocationError
  = LocationNotFound
  | LocationNotUpdated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data RouteError
  = RouteNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data DocumentError
  = InvalidPassApplicationId
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data HealthCheckError
  = HealthCheckNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data OrganisationError
  = OrganisationNotFound
  | OrganisationIdMissing
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data CaseError
  = CaseNotFound
  | CaseNotCreated
  | CaseNotUpdated
  | CaseStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data ProductInstanceError
  = ProductInstanceNotFound
  | ProductInstanceStatusTransitionErr ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data ProductError
  = ProductNotFound
  | ProductNotUpdated
  | ProductNotCreated
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data ProductInfoError
  = ProductInfoNotFound
  | ProductInfoNotUpdated
  | OtherProductInfoError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

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
