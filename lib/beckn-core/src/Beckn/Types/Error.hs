{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Error where

import EulerHS.Prelude

newtype ErrorCode = ErrorCode Int deriving (Generic, Eq, Show, FromJSON, ToJSON)

newtype ErrorMsg = ErrorMsg Text deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

data DomainError = 
   EpassErr EpassError
 | QuotaErr QuotaError
 | CommentErr CommentError
 | HealthCheckErr HealthCheckError
 | BlacklistErr BlacklistError
 | TagErr TagError
 | OrganisationErr OrganisationError
 | UnknownDomainError
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data EpassError = 
    EpassNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data QuotaError = 
    EpassNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)


data CommentError = 
   CommentNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)


data HealthCheckError = 
   CommentNotFound
   deriving (Generic, Eq, Show, FromJSON, ToJSON)


data BlacklistError = 
   CommentNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)


data TagError = 
   CommentNotFound
   deriving (Generic, Eq, Show, FromJSON, ToJSON)


data OrganisationError = 
   CommentNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)


data SystemError = 
     MethodNotAllowed
 deriving (Generic, Eq, Show, FromJSON, ToJSON)

data BecknError
  = BecknError
      { _errorCode :: ErrorCode,
        _errorMessage :: ErrorMsg
      }
  deriving (Generic)

instance FromJSON BecknError where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON BecknError where
  toJSON = genericToJSON stripLensPrefixOptions
