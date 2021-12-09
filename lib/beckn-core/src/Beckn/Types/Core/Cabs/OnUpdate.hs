module Beckn.Types.Core.Cabs.OnUpdate
  ( module Beckn.Types.Core.Cabs.OnUpdate,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnUpdate.OnUpdateEvent as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnUpdateMessage = OnUpdateMessage
  { cabs_update_event :: OnUpdateEvent
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
