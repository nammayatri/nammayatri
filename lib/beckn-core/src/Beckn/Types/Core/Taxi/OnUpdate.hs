module Beckn.Types.Core.Taxi.OnUpdate
  ( module Beckn.Types.Core.Taxi.OnUpdate,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnUpdateMessage = OnUpdateMessage
  { cabs_update_event :: OnUpdateEvent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
