module Beckn.Types.Core.Migration.Page (Page (..)) where

import EulerHS.Prelude

data Page = Page
  { _id :: Maybe Text,
    _next_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Page where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Page where
  toJSON = genericToJSON stripLensPrefixOptions
