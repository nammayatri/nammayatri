module Beckn.Types.Core.Error where

import Beckn.Utils.JSON (constructorsWithHyphens)
import EulerHS.Prelude

data Error = Error
  { _type :: ErrorType,
    _code :: Text,
    _path :: Maybe Text,
    _message :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | INTERNAL_ERROR
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving (Generic, Show, Eq)

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Error where
  toJSON = genericToJSON stripAllLensPrefixOptions
