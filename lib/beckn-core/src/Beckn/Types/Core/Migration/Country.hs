module Beckn.Types.Core.Migration.Country (Country (..)) where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Country = Country
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Country where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Country where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
