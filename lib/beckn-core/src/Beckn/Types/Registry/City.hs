module Beckn.Types.Registry.City (City (..)) where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import EulerHS.Prelude

data City = City
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON City where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON City where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
