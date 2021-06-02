module Beckn.Types.Core.Rating where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Rating = Rating
  { value :: Text,
    unit :: Text, -- Default:U+2B50 Follows the unicode 13.0 format for emojis : https://unicode.org/emoji/charts/full-emoji-list.html
    max_value :: Maybe Text,
    direction :: Maybe Text -- Default "UP" - "UP", "DOWN"
  }
  deriving (Generic, Show)

instance FromJSON Rating where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Rating where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Rating where
  example =
    Rating
      { value = "5",
        unit = "U+2B50",
        max_value = Just "5",
        direction = Just "UP"
      }
