{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RecurringBooking where

import Data.Set (Set)
import Data.Time.Calendar (Day, DayOfWeek)
import Database.Esqueleto.PostgreSQL.JSON (JSONB (..))
import qualified Domain.Types.RecurringBooking as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Booking.BookingLocation as SLoc
import qualified Storage.Tabular.FarePolicy as SFarePolicy

derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RecurringBookingT sql=recurring_booking
      id Text

      scheduledDays (JSONB (Set DayOfWeek))
      startDate Day
      endDate Day Maybe
      pickupTime TimeOfDay
      status Domain.Status
      providerId Text
      bapId Text
      bapUri Text
      farePolicyId SFarePolicy.FarePolicyTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId

      Primary id
      deriving Generic
  |]

instance TEntityKey RecurringBookingT where
  type DomainKey RecurringBookingT = Id Domain.RecurringBooking
  fromKey (RecurringBookingTKey _id) = Id _id
  toKey (Id id) = RecurringBookingTKey id

instance FromTType RecurringBookingT Domain.SimpleRecurringBooking where
  fromTType (RecurringBookingT {..}) = do
    pBapUrl <- parseBaseUrl bapUri
    let (JSONB scheduledDays') = scheduledDays
    pure $
      Domain.SimpleRecurringBooking
        { id = Id id,
          scheduledDays = scheduledDays',
          startDate = startDate,
          endDate = endDate,
          pickupTime = pickupTime,
          status = status,
          providerId = providerId,
          bapId = bapId,
          bapUri = pBapUrl,
          farePolicyId = fromKey farePolicyId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey toLocationId
        }

instance ToTType RecurringBookingT Domain.SimpleRecurringBooking where
  toTType booking =
    RecurringBookingT
      { id = getId booking.id,
        scheduledDays = JSONB booking.scheduledDays,
        startDate = booking.startDate,
        endDate = booking.endDate,
        pickupTime = booking.pickupTime,
        status = booking.status,
        providerId = booking.providerId,
        farePolicyId = toKey booking.farePolicyId,
        bapId = booking.bapId,
        bapUri = showBaseUrl booking.bapUri,
        toLocationId = toKey booking.toLocationId,
        fromLocationId = toKey booking.fromLocationId
      }

type FullRecurringBookingT = (RecurringBookingT, SLoc.BookingLocationT, SLoc.BookingLocationT)

instance FromTType FullRecurringBookingT Domain.RecurringBooking where
  fromTType (RecurringBookingT {..}, fromLoc, toLoc) = do
    fromLocation <- fromTType fromLoc
    toLocation <- fromTType toLoc
    pBapUrl <- parseBaseUrl bapUri
    let (JSONB scheduledDays') = scheduledDays
    pure $
      Domain.RecurringBooking
        { id = Id id,
          scheduledDays = scheduledDays',
          startDate = startDate,
          endDate = endDate,
          pickupTime = pickupTime,
          status = status,
          providerId = providerId,
          bapId = bapId,
          bapUri = pBapUrl,
          farePolicyId = fromKey farePolicyId,
          fromLocation = fromLocation,
          toLocation = toLocation
        }

instance ToTType FullRecurringBookingT Domain.RecurringBooking where
  toTType booking =
    let toLocationT = toTType booking.toLocation
        fromLocationT = toTType booking.fromLocation
        bookingT =
          RecurringBookingT
            { id = getId booking.id,
              scheduledDays = JSONB booking.scheduledDays,
              startDate = booking.startDate,
              endDate = booking.endDate,
              pickupTime = booking.pickupTime,
              status = booking.status,
              providerId = booking.providerId,
              farePolicyId = toKey booking.farePolicyId,
              bapId = booking.bapId,
              bapUri = showBaseUrl booking.bapUri,
              toLocationId = toKey booking.toLocation.id,
              fromLocationId = toKey booking.fromLocation.id
            }
     in (bookingT, toLocationT, fromLocationT)
