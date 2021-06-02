module Beckn.Types.Core.Migration.Language where

import Beckn.Utils.JSON
import EulerHS.Prelude

newtype Language = Language
  { code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Language where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Language where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
