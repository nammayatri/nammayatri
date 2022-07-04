{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking.RentalRideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyTId)
import Storage.Tabular.RideBooking.Table

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalRideBookingT sql=rental_ride_booking
      rideBookingId RideBookingTId
      rentalFarePolicyId RentalFarePolicyTId
      Primary rideBookingId
      deriving Generic
    |]

instance TEntityKey RentalRideBookingT where
  type DomainKey RentalRideBookingT = Id Domain.RideBooking
  fromKey (RentalRideBookingTKey _id) = fromKey _id
  toKey id = RentalRideBookingTKey $ toKey id
