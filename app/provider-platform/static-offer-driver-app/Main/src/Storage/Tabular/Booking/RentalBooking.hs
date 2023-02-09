{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.RentalBooking where

import qualified Domain.Types.Booking.Type as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Booking.Table
import Storage.Tabular.FarePolicy.RentalFarePolicy (RentalFarePolicyTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalBookingT sql=rental_booking
      bookingId BookingTId
      rentalFarePolicyId RentalFarePolicyTId
      Primary bookingId
      deriving Generic
    |]

instance TEntityKey RentalBookingT where
  type DomainKey RentalBookingT = Id Domain.Booking
  fromKey (RentalBookingTKey _id) = fromKey _id
  toKey id = RentalBookingTKey $ toKey id
