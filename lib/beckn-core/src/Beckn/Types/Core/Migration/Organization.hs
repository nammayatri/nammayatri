module Beckn.Types.Core.Migration.Organization (Organization (..)) where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Organization = Organization
  { name :: Maybe Text,
    cred :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Organization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Organization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
