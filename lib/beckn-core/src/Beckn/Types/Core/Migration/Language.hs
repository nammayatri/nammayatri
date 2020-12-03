module Beckn.Types.Core.Migration.Language where

import EulerHS.Prelude

newtype Language = Language
  { _code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Language where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Language where
  toJSON = genericToJSON stripLensPrefixOptions
