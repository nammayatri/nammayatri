module Beckn.Types.Registry.City (City (..)) where

import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data City = City
  { name :: Maybe Text,
    code :: Maybe Text
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON City where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON City where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
