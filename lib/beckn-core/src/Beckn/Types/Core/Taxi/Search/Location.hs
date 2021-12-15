module Beckn.Types.Core.Taxi.Search.Location where

import Beckn.Types.Core.Taxi.Search.Address (Address)
import Beckn.Types.Core.Taxi.Search.Gps (Gps)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Location = Location
  { gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example Location where
  example =
    Location
      { gps = example,
        address = example
      }
