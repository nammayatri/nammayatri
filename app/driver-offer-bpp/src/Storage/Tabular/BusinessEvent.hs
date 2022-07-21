{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BusinessEvent where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

derivePersistField "Domain.EventType"
derivePersistField "Domain.WhenPoolWasComputed"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BusinessEventT sql=business_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.EventType
      timeStamp UTCTime
      bookingId BookingTId Maybe
      whenPoolWasComputed Domain.WhenPoolWasComputed Maybe
      vehicleVariant Variant Maybe
      distance Int Maybe
      duration Int Maybe
      rideId RideTId Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey BusinessEventT where
  type DomainKey BusinessEventT = Id Domain.BusinessEvent
  fromKey (BusinessEventTKey _id) = Id _id
  toKey (Id id) = BusinessEventTKey id

instance TType BusinessEventT Domain.BusinessEvent where
  fromTType BusinessEventT {..} = do
    return $
      Domain.BusinessEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          bookingId = fromKey <$> bookingId,
          rideId = fromKey <$> rideId,
          distance = Meters <$> distance,
          duration = Seconds <$> duration,
          ..
        }
  toTType Domain.BusinessEvent {..} =
    BusinessEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        bookingId = toKey <$> bookingId,
        rideId = toKey <$> rideId,
        distance = getMeters <$> distance,
        duration = getSeconds <$> duration,
        ..
      }
