module Beckn.Types.Core.Migration1.Search.Tags (Tags (..)) where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Tags = Tags
  { distance :: Double
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Tags where
  example =
    Tags
      { distance = 12
      }
