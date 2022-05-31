{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RideBooking.RentalRideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.RideBooking
import Domain.Types.RideBooking.RentalRideBooking
import Storage.Tabular.RideBooking.RentalRideBooking

findByRideBookingId :: Transactionable m => Id RideBooking -> m (Maybe RentalRideBooking)
findByRideBookingId rideBookingId =
  findOne $ do
    rentalRideBooking <- from $ table @RentalRideBookingT
    where_ $ rentalRideBooking ^. RentalRideBookingRideBookingId ==. val (getId rideBookingId)
    return rentalRideBooking
