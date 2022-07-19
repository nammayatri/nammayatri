module Beckn.Types.Core.Taxi.Select
  ( module Beckn.Types.Core.Taxi.Select,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Search.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SelectMessage = SelectMessage
  { intent :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
