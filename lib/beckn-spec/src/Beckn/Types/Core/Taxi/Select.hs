module Beckn.Types.Core.Taxi.Select
  ( module Beckn.Types.Core.Taxi.Select,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Select.Descriptor as Reexport
import Beckn.Types.Core.Taxi.Select.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Select.Location as Reexport
import Beckn.Types.Core.Taxi.Select.Order as Reexport
import Beckn.Types.Core.Taxi.Select.Payment as Reexport
import Beckn.Types.Core.Taxi.Select.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Select.StopInfo as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype SelectMessage = SelectMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
