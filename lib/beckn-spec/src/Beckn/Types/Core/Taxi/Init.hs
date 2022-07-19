module Beckn.Types.Core.Taxi.Init
  ( module Beckn.Types.Core.Taxi.Init,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Init.Descriptor as Reexport
import Beckn.Types.Core.Taxi.Init.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Init.Location as Reexport
import Beckn.Types.Core.Taxi.Init.Order as Reexport
import Beckn.Types.Core.Taxi.Init.Payment as Reexport
import Beckn.Types.Core.Taxi.Init.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Init.StopInfo as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype InitMessage = InitMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
