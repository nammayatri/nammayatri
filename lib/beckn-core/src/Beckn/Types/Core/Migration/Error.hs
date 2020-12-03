module Beckn.Types.Core.Migration.Error
  ( Error (..),
    ErrorType (..),
    domainError,
  )
where

import Beckn.Utils.JSON (constructorsWithHyphens)
import EulerHS.Prelude

data Error = Error
  { _type :: ErrorType,
    _code :: Text,
    _path :: Maybe Text,
    _message :: Maybe Text
  }
  deriving (Generic, Show)

domainError :: Text -> Error
domainError err = Error DOMAIN_ERROR err Nothing Nothing

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving (Generic, Show)

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Error where
  toJSON = genericToJSON stripAllLensPrefixOptions
