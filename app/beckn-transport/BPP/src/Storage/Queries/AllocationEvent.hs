module Storage.Queries.AllocationEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.AllocationEvent
import Domain.Types.Booking
import Domain.Types.Person (Driver, Person)
import Storage.Tabular.AllocationEvent

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

deleteByPersonId :: Id Driver -> SqlDB ()
deleteByPersonId driverId =
  Esq.delete $ do
    allocationEvents <- from $ table @AllocationEventT
    where_ $ allocationEvents ^. AllocationEventDriverId ==. just (val . toKey . cast @Driver @Person $ driverId)
