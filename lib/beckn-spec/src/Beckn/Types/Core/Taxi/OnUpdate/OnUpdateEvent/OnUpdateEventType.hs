module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data OnUpdateEventType
  = RIDE_COMPLETED
  | RIDE_STARTED
  | RIDE_ASSIGNED
  | RIDE_BOOKING_CANCELLED
  | RIDE_BOOKING_REALLOCATION
  | DRIVER_ARRIVED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
