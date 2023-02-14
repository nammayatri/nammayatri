 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.AllocationEvent where

import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id (Id)

data AllocationEvent = AllocationEvent
  { id :: Id AllocationEvent,
    driverId :: Maybe (Id Driver),
    eventType :: AllocationEventType,
    timestamp :: UTCTime,
    bookingId :: Id DRB.Booking
  }
  deriving (Generic)

data AllocationEventType
  = NotificationSent
  | MarkedAsAccepted
  | MarkedAsRejected
  | MarkedAsIgnored
  | AcceptedByDriver
  | RejectedByDriver
  | ConsumerCancelled
  | EmptyDriverPool
  | AllocationTimeFinished
  | ReallocationLimitExceed
  | BatchLimitExceed
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)
