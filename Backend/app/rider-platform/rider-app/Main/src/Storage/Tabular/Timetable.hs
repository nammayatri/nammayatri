{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Timetable where

import Data.Time.Calendar (Day)
import qualified Domain.Types.Timetable as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified Storage.Tabular.RecurringBooking as SRBooking

derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TimetableT sql=timetable
        id Text
        recurringBookingId SRBooking.RecurringBookingTId
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

instance TType TimetableT Domain.Timetable where
  toTType timetable =
    TimetableT
      { id = getId timetable.id,
        recurringBookingId = toKey timetable.recurringBookingId,
        pickupDate = timetable.pickupDate,
        pickupTime = timetable.pickupTime,
        status = timetable.status
      }

  fromTType timetableT =
    pure $
      Domain.Timetable
        { id = Id (id timetableT),
          recurringBookingId = fromKey (recurringBookingId timetableT),
          pickupDate = pickupDate timetableT,
          pickupTime = pickupTime timetableT,
          status = status timetableT
        }
