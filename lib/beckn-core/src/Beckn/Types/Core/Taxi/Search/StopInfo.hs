module Beckn.Types.Core.Taxi.Search.StopInfo where

import Beckn.Types.Core.Taxi.Search.Location (Location)
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

newtype StopInfo = StopInfo
  { location :: Location
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Example StopInfo where
  example =
    StopInfo
      { location = example
      }
