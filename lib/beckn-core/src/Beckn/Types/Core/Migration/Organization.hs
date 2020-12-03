module Beckn.Types.Core.Migration.Organization (Organization (..)) where

import EulerHS.Prelude

data Organization = Organization
  { _name :: Maybe Text,
    _cred :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Organization where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Organization where
  toJSON = genericToJSON stripLensPrefixOptions
