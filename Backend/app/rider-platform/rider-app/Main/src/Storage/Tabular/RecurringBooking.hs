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
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.Booking.BookingLocation as SLoc
import qualified Storage.Tabular.Merchant as SMerchant
import qualified Storage.Tabular.Person as SPerson

derivePersistField "Domain.Status"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RecurringBookingT sql=recurring_booking
      id Text
      merchantId SMerchant.MerchantTId

      scheduledDays (JSONB (Set DayOfWeek))
      startDate Day
      endDate Day Maybe
      pickupTime TimeOfDay
      status Domain.Status
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      riderId SPerson.PersonTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId
      distance HighPrecMeters
      vehicleVariant VehVar.VehicleVariant

      Primary id
      deriving Generic
  |]

instance TEntityKey RecurringBookingT where
  type DomainKey RecurringBookingT = Id Domain.RecurringBooking
  fromKey (RecurringBookingTKey _id) = Id _id
  toKey (Id id) = RecurringBookingTKey id

type FullRecurringBookingT = (RecurringBookingT, SLoc.BookingLocationT, SLoc.BookingLocationT)

instance FromTType FullRecurringBookingT Domain.RecurringBooking where
  fromTType (RecurringBookingT {..}, fromLoc, toLoc) = do
    pUrl <- parseBaseUrl providerUrl
    fromLocation <- fromTType fromLoc
    toLocation <- fromTType toLoc
    let (JSONB scheduledDays') = scheduledDays
    pure $
      Domain.RecurringBooking
        { id = Id id,
          merchantId = fromKey merchantId,
          scheduledDays = scheduledDays',
          startDate = startDate,
          endDate = endDate,
          pickupTime = pickupTime,
          status = status,
          providerId = providerId,
          providerUrl = pUrl,
          providerName = providerName,
          providerMobileNumber = providerMobileNumber,
          riderId = fromKey riderId,
          fromLocation = fromLocation,
          toLocation = toLocation,
          distance = distance,
          vehicleVariant = vehicleVariant
        }

instance ToTType FullRecurringBookingT Domain.RecurringBooking where
  toTType booking =
    let toLocationT = toTType booking.toLocation
        fromLocationT = toTType booking.fromLocation
        bookingT =
          RecurringBookingT
            { id = getId booking.id,
              merchantId = toKey booking.merchantId,
              scheduledDays = JSONB booking.scheduledDays,
              startDate = booking.startDate,
              endDate = booking.endDate,
              pickupTime = booking.pickupTime,
              status = booking.status,
              providerId = booking.providerId,
              providerUrl = showBaseUrl booking.providerUrl,
              providerName = booking.providerName,
              providerMobileNumber = booking.providerMobileNumber,
              riderId = toKey booking.riderId,
              toLocationId = toKey booking.toLocation.id,
              fromLocationId = toKey booking.fromLocation.id,
              distance = booking.distance,
              vehicleVariant = booking.vehicleVariant
            }
     in (bookingT, toLocationT, fromLocationT)
