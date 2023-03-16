{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Timetable where

import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime (..))
import qualified Domain.Types.RecurringBooking as DRecurringBooking
import qualified Domain.Types.Timetable as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.Booking as SBooking
import qualified Storage.Tabular.FarePolicy.FarePolicy as SFarePolicy
import qualified Storage.Tabular.RecurringBooking as SRBooking

derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TimetableT sql=timetable
        id Text
        recurringBookingId SRBooking.RecurringBookingTId
        bookingId SBooking.BookingTId Maybe
        pickupDate Day
        pickupTime TimeOfDay
        status Domain.Status

        Primary id
        UniqueBookingPickupDate recurringBookingId pickupDate
        deriving Generic
|]

instance TEntityKey TimetableT where
  type DomainKey TimetableT = Id Domain.Timetable
  fromKey (TimetableTKey _id) = Id _id
  toKey (Id id) = TimetableTKey id

instance ToTType TimetableT Domain.Timetable where
  toTType timetable =
    TimetableT
      { id = getId timetable.id,
        recurringBookingId = toKey timetable.recurringBookingId,
        bookingId = fmap toKey timetable.bookingId,
        pickupDate = localDay timetable.pickupTime,
        pickupTime = localTimeOfDay timetable.pickupTime,
        status = timetable.status
      }

instance FromTType TimetableT Domain.Timetable where
  fromTType timetableT =
    pure $
      Domain.Timetable
        { id = Id (id timetableT),
          recurringBookingId = fromKey (recurringBookingId timetableT),
          bookingId = fmap fromKey (bookingId timetableT),
          pickupTime = LocalTime (pickupDate timetableT) (pickupTime timetableT),
          status = status timetableT
        }

instance
  TType
    ( TimetableT,
      SRBooking.FullRecurringBookingT,
      SFarePolicy.FarePolicyT
    )
    Domain.UpcomingBooking
  where
  fromTType (timetableT, bookingT, farePolicyT) = do
    tt <- fromTType @_ @Domain.Timetable timetableT
    DRecurringBooking.RecurringBooking {id = recurringBookingId, ..} <- fromTType bookingT
    farePolicy <- fromTType farePolicyT
    pure $ Domain.UpcomingBooking {id = tt.id, pickupTime = tt.pickupTime, bookingId = Nothing, ..}
  toTType _ =
    error "this is a read model"
