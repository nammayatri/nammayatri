module Storage.Queries.AllocationEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.AllocationEvent
import Domain.Types.Booking
import Storage.Tabular.AllocationEvent ()
import Types.App (Driver)
import Utils.Common

logAllocationEvent :: AllocationEventType -> Id Booking -> Maybe (Id Driver) -> SqlDB ()
logAllocationEvent eventType bookingId driverId = do
  uuid <- generateGUID
  now <- getCurrentTime
  Esq.create $
    AllocationEvent
      { id = uuid,
        eventType = eventType,
        timestamp = now,
        driverId = driverId,
        bookingId = bookingId
      }
