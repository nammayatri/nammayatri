module Beckn.Types.Core.Error where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Error = Error
  { _type :: ErrorType,
    code :: Text,
    path :: Maybe Text,
    message :: Maybe Text
  }
  deriving (Generic, Show, Eq)

data ErrorType
  = CONTEXT_ERROR
  | CORE_ERROR
  | INTERNAL_ERROR -- Not a spec value. TODO: get rid of it.
  | DOMAIN_ERROR
  | POLICY_ERROR
  | JSON_SCHEMA_ERROR
  deriving (Generic, Show, Eq)

instance FromJSON ErrorType where
  parseJSON = genericParseJSON constructorsWithHyphens

instance ToJSON ErrorType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON Error where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Error where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
