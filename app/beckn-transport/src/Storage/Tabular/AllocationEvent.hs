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
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.AllocationEventType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AllocationEventT sql=allocation_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.AllocationEventType
      timestamp UTCTime
      rideBookingId RideBookingTId
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
          rideBookingId = fromKey rideBookingId,
          ..
        }
  toTType Domain.AllocationEvent {..} =
    AllocationEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        rideBookingId = toKey rideBookingId,
        ..
      }
