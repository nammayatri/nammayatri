module Beckn.Types.Core.Migration.Contact (Contact (..)) where

import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Contact = Contact
  { phone :: Maybe Text,
    email :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Contact where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Contact where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
