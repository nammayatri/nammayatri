module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideBookingCancelledEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent as Reexport
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent as Reexport
import qualified Beckn.Utils.JSON as J
import qualified Beckn.Utils.Schema as S
import Data.OpenApi
import EulerHS.Prelude hiding ((.=))

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  | RideBookingCancelled RideBookingCancelledEvent
  deriving (Generic, Show)

instance ToJSON OnUpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnUpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
