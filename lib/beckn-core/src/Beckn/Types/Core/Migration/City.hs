module Beckn.Types.Core.Migration.City (City (..)) where

import EulerHS.Prelude

data City = City
  { _name :: Maybe Text,
    _code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON City where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON City where
  toJSON = genericToJSON stripAllLensPrefixOptions
