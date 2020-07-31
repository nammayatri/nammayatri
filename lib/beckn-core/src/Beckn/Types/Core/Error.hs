module Beckn.Types.Core.Error where

import EulerHS.Prelude

data Error = Error
  { _type :: Text, -- "CONTEXT-ERROR", "CORE-ERROR", "DOMAIN-ERROR", "POLICY-ERROR", "JSON-SCHEMA-ERROR"
    _code :: Text,
    _path :: Maybe Text,
    _message :: Maybe Text
  }
  deriving (Generic, Show)

domainError :: Text -> Error
domainError err = Error "DOMAIN-ERROR" err Nothing Nothing

instance FromJSON Error where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Error where
  toJSON = genericToJSON stripAllLensPrefixOptions
