module Beckn.Types.Core.Cabs.OnUpdate
  ( module Beckn.Types.Core.Cabs.OnUpdate,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnUpdate.RideOrder as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnUpdateMessage = OnUpdateMessage
  { order :: RideOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
