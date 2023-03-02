{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.AllocationEvent where

import Domain.Types.AllocationEvent
import Domain.Types.Booking
import Domain.Types.Person (Driver, Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.AllocationEvent

logAllocationEvent :: AllocationEventType -> Id Booking -> Maybe (Id Driver) -> SqlDB m ()
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

deleteByPersonId :: Id Driver -> SqlDB m ()
deleteByPersonId driverId =
  Esq.delete $ do
    allocationEvents <- from $ table @AllocationEventT
    where_ $ allocationEvents ^. AllocationEventDriverId ==. just (val . toKey . cast @Driver @Person $ driverId)
