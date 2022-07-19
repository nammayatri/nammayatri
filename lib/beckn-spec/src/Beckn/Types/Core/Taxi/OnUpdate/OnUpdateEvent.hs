module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingReallocationEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent
import qualified Beckn.Utils.JSON as J
import qualified Beckn.Utils.Schema as S
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  | RideBookingCancelled RideBookingCancelledEvent
  | RideBookingReallocation RideBookingReallocationEvent
  deriving (Generic, Show)

instance ToJSON OnUpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnUpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
