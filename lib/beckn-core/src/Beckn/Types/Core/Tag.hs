module Beckn.Types.Core.Tag where

import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Tag = Tag
  { key :: Text,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance Example Tag where
  example =
    Tag
      { key = "key",
        value = "value"
      }
