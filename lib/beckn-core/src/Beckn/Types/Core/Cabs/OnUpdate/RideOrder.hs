module Beckn.Types.Core.Cabs.OnUpdate.RideOrder
  ( module Beckn.Types.Core.Cabs.OnUpdate.RideOrder,
    module Reexport,
  )
where

import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideCompletedOrder as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideOrderStatus as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.RideStartedOrder as Reexport
import Beckn.Types.Core.Cabs.OnUpdate.RideOrder.TripAssignedOrder as Reexport
import Beckn.Utils.Schema (untaggedValue)
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))

data RideOrder
  = TripAssigned TripAssignedOrder
  | RideStarted RideStartedOrder
  | RideCompleted RideCompletedOrder
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema RideOrder where
  declareNamedSchema = genericDeclareNamedSchema untaggedValue