{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent,
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingCancelledEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.BookingReallocationEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.DriverArrivedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.NewMessageEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideAssignedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideStartedEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.SafetyAlertEvent
import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.StopArrivedEvent
import Data.OpenApi
import EulerHS.Prelude hiding (fromList)
import qualified Kernel.Utils.JSON as J
import qualified Kernel.Utils.Schema as S

data OnUpdateEvent
  = RideAssigned RideAssignedEvent
  | RideStarted RideStartedEvent
  | RideCompleted RideCompletedEvent
  | BookingCancelled BookingCancelledEvent
  | BookingReallocation BookingReallocationEvent
  | DriverArrived DriverArrivedEvent
  | EstimateRepetition EstimateRepetitionEvent
  | NewMessage NewMessageEvent
  | SafetyAlert SafetyAlertEvent
  | StopArrived StopArrivedEvent
  deriving (Generic, Show)

instance ToJSON OnUpdateEvent where
  toJSON = genericToJSON J.untaggedValue

instance FromJSON OnUpdateEvent where
  parseJSON = genericParseJSON J.untaggedValue

instance ToSchema OnUpdateEvent where
  declareNamedSchema = genericDeclareNamedSchema S.untaggedValue
