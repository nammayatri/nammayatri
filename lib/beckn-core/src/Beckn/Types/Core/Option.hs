module Beckn.Types.Core.Option where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text
import EulerHS.Prelude hiding (id)

data Option = Option
  { id :: Text,
    descriptor :: Descriptor
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example Option where
  example =
    Option
      { id = idExample,
        descriptor = example
      }
