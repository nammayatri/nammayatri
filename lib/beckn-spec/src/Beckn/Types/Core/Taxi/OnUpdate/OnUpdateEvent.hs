module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  | BookingCancelled BookingCancelledEvent
  | BookingReallocation BookingReallocationEvent
  | DriverArrived DriverArrivedEvent
  deriving (Generic, Show)

instance ToJSON OnUpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnUpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
