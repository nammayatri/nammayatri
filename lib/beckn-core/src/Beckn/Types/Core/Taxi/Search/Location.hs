module Beckn.Types.Core.Taxi.Search.Location where

import Beckn.Types.Core.Taxi.Search.Gps (Gps)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype Location = Location
  { gps :: Gps
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example Location where
  example =
    Location
      { gps = example
      }
