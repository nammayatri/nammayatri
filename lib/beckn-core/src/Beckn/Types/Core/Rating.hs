module Beckn.Types.Core.Rating where

import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Rating = Rating
  { _value :: Text,
    _unit :: Text, -- Default:U+2B50 Follows the unicode 13.0 format for emojis : https://unicode.org/emoji/charts/full-emoji-list.html
    _max_value :: Maybe Text,
    _direction :: Maybe Text -- Default "UP" - "UP", "DOWN"
  }
  deriving (Generic, Show)

instance FromJSON Rating where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Rating where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Rating where
  example =
    Rating
      { _value = "5",
        _unit = "U+2B50",
        _max_value = Just "5",
        _direction = Just "UP"
      }
