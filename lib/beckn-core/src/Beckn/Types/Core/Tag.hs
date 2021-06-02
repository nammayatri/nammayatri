module Beckn.Types.Core.Tag where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Tag = Tag
  { key :: Text,
    value :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Tag where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Tag where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Tag where
  example =
    Tag
      { key = "key",
        value = "value"
      }
