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
import qualified Domain.Types.RideBooking.RentalRideBooking as Domain
import Storage.Tabular.RentalFarePolicy (RentalFarePolicyTId)

-- FIXME rideBookingId should be RideBookingTId, but I made it Text to avoid cyclic dependencies
mkPersist
  defaultSqlSettings
  [defaultQQ|
    RentalRideBookingT sql=rental_ride_booking
      rideBookingId Text
      rentalFarePolicyId RentalFarePolicyTId
      Primary rideBookingId
      deriving Generic
    |]

instance TEntityKey RentalRideBookingT where
  type DomainKey RentalRideBookingT = Id Domain.RideBooking
  fromKey (RentalRideBookingTKey _id) = Id _id
  toKey (Id id) = RentalRideBookingTKey id

instance TEntity RentalRideBookingT Domain.RentalRideBooking where
  fromTEntity entity = do
    let RentalRideBookingT {..} = entityVal entity
    return $
      Domain.RentalRideBooking
        { rideBookingId = Id rideBookingId,
          rentalFarePolicyId = fromKey rentalFarePolicyId
        }
  toTType Domain.RentalRideBooking {..} =
    RentalRideBookingT
      { rideBookingId = getId rideBookingId,
        rentalFarePolicyId = toKey rentalFarePolicyId
      }
  toTEntity a =
    Entity (toKey a.rideBookingId) $ toTType a
