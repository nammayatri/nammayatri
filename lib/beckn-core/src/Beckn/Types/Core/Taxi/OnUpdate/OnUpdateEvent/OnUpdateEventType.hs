module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data OnUpdateEventType
  = RIDE_COMPLETED
  | RIDE_STARTED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
