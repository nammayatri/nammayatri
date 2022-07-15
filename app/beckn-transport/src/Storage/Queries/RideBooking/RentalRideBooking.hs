{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RideBooking.RentalRideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking.Type
import Storage.Tabular.RideBooking ()
import Storage.Tabular.RideBooking.RentalRideBooking

findByRideBookingId' :: Transactionable m => Id RideBooking -> DTypeBuilder m (Maybe RentalRideBookingT)
findByRideBookingId' rideBookingId =
  Esq.findOne' $ do
    rentalRideBooking <- from $ table @RentalRideBookingT
    where_ $ rentalRideBooking ^. RentalRideBookingRideBookingId ==. val (toKey rideBookingId)
    return rentalRideBooking
