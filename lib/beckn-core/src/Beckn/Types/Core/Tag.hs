module Beckn.Types.Core.Tag where

import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Tag = Tag
  { _key :: Text,
    _value :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Tag where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tag where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Tag where
  example =
    Tag
      { _key = "key",
        _value = "value"
      }
