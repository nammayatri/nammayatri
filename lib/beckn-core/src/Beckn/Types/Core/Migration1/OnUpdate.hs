module Beckn.Types.Core.Migration1.OnUpdate
  ( module Beckn.Types.Core.Migration1.OnUpdate,
    module Reexport,
  )
where

import Beckn.Types.Core.Migration1.OnUpdate.RideOrder as Reexport
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

newtype OnUpdateMessage = OnUpdateMessage
  { order :: RideOrder
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
