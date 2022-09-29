{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.AllocationEvent where

import Beckn.Types.Id (Id)
import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)

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
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON)
