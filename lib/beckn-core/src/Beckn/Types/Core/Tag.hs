module Beckn.Types.Core.Tag where

import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Tag = Tag
  { _label :: Text,
    _value :: Text
  }
  deriving (Generic, Show)

instance FromJSON Tag where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tag where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Tag where
  example =
    Tag
      { _label = "key",
        _value = "value"
      }
