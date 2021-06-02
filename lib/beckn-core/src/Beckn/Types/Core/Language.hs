module Beckn.Types.Core.Language where

import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

newtype Language = Language
  { code :: Text
  }
  deriving (Generic, Show)

instance FromJSON Language where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Language where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
