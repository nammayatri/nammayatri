{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import EulerHS.Prelude

data Action = ACK | NACK deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

newtype ErrorCode = ErrorCode Int
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

newtype ErrorMsg = ErrorMsg Text
  deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

data DomainError
  = EpassErr EpassError
  | QuotaErr QuotaError
  | CommentErr CommentError
  | HealthCheckErr HealthCheckError
  | BlacklistErr BlacklistError
  | TagErr TagError
  | OrganisationErr OrganisationError
  | UnknownDomainError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data EpassError
  = EpassNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data QuotaError
  = QuotaNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data CommentError
  = CommentNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data HealthCheckError
  = HealthCheckNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data BlacklistError
  = BlacklistNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data TagError
  = TagNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data OrganisationError
  = OrganisationNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data SystemError = SystemError ErrorMsg
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data BecknError = BecknError
  { _errorCode :: ErrorCode,
    _errorMessage :: ErrorMsg,
    _action :: Action
  }
  deriving (Generic)

instance FromJSON BecknError where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON BecknError where
  toJSON = genericToJSON stripLensPrefixOptions
