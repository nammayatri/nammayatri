module Beckn.Types.Core.Migration.Page (Page (..)) where

import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)

data Page = Page
  { id :: Maybe Text,
    next_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Page where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Page where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
