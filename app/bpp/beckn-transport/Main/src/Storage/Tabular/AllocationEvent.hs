{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AllocationEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.AllocationEvent as Domain
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

instance TType AllocationEventT Domain.AllocationEvent where
  fromTType AllocationEventT {..} = do
    return $
      Domain.AllocationEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          bookingId = fromKey bookingId,
          ..
        }
  toTType Domain.AllocationEvent {..} =
    AllocationEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        bookingId = toKey bookingId,
        ..
      }
