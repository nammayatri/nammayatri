{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType where

import Kernel.Prelude

data OnUpdateEventType
  = RIDE_ENDED
  | RIDE_STARTED
  | RIDE_ASSIGNED
  | RIDE_CANCELLED
  | RIDE_ARRIVED_PICKUP
  | RIDE_BOOKING_REALLOCATION
  | ESTIMATE_REPETITION
  | QUOTE_REPETITION
  | NEW_MESSAGE
  | SAFETY_ALERT
  | PHONE_CALL_REQUEST
  | PHONE_CALL_COMPLETED
  | STOP_ARRIVED
  | TOLL_CROSSED
  | ESTIMATED_END_TIME_RANGE_UPDATED
  | PARCEL_IMAGE_UPLOADED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
