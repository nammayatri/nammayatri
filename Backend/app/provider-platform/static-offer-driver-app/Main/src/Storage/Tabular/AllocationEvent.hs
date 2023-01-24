{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AllocationEvent where

import qualified Domain.Types.AllocationEvent as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.AllocationEventType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AllocationEventT sql=allocation_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.AllocationEventType
      timestamp UTCTime
      bookingId BookingTId
      Primary id
      deriving Generic
    |]

instance TEntityKey AllocationEventT where
  type DomainKey AllocationEventT = Id Domain.AllocationEvent
  fromKey (AllocationEventTKey _id) = Id _id
  toKey (Id id) = AllocationEventTKey id

instance FromTType AllocationEventT Domain.AllocationEvent where
  fromTType AllocationEventT {..} = do
    return $
      Domain.AllocationEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          bookingId = fromKey bookingId,
          ..
        }

instance ToTType AllocationEventT Domain.AllocationEvent where
  toTType Domain.AllocationEvent {..} =
    AllocationEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        bookingId = toKey bookingId,
        ..
      }
