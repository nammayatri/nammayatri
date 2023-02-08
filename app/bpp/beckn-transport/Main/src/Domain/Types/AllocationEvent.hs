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
