module Beckn.Types.Core.Option where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Option = Option
  { id :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example Option where
  example =
    Option
      { id = idExample,
        descriptor = example
      }
