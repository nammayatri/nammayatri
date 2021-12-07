module Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideOrderStatus where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data RideOrderStatus
  = COMPLETED
  | STARTED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
