module Beckn.Types.Registry.Country (Country (..)) where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import EulerHS.Prelude
import Data.OpenApi (ToSchema)

data Country = Country
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Country where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Country where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
