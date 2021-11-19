module Beckn.Types.Core.Migration1.OnSearch.Tags (Tags (..)) where

import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Tags = Tags
  { contacts :: Text,
    rides_inprogress :: Int,
    rides_completed :: Int,
    rides_confirmed :: Int
  }
  deriving (Eq, Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Tags where
  example =
    Tags
      { contacts = "99999999999",
        rides_inprogress = 12,
        rides_completed = 32,
        rides_confirmed = 3
      }
