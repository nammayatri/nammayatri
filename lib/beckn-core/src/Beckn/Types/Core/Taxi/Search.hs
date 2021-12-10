module Beckn.Types.Core.Taxi.Search
  ( module Beckn.Types.Core.Taxi.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Search.Gps as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Beckn.Types.Core.Taxi.Search.Time as Reexport
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SearchMessage = SearchMessage
  { intent :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example SearchMessage where
  example =
    SearchMessage
      { intent = example
      }
