module Beckn.Types.Core.Migration.Contact (Contact (..)) where

import Beckn.Types.Core.Migration.Tags (Tags)
import EulerHS.Prelude

data Contact = Contact
  { _phone :: Maybe Text,
    _email :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

instance FromJSON Contact where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Contact where
  toJSON = genericToJSON stripLensPrefixOptions
