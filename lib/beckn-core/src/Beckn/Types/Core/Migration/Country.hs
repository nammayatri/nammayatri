module Beckn.Types.Core.Migration.Country (Country (..)) where

import EulerHS.Prelude

data Country = Country
  { _name :: Maybe Text,
    _code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Country where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Country where
  toJSON = genericToJSON stripAllLensPrefixOptions
