module Beckn.Types.Core.Migration1.OnUpdate.RideOrder.RideOrderStatus where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data RideOrderStatus
  = COMPLETED
  | STARTED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
