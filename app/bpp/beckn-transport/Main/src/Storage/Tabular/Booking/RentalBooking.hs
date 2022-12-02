{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.RentalBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Booking.Type as Domain
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
