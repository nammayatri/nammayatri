{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking.RentalBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking.Type
import Storage.Tabular.Booking ()
import Storage.Tabular.Booking.RentalBooking

findByBookingId' :: Transactionable m => Id Booking -> DTypeBuilder m (Maybe RentalBookingT)
findByBookingId' bookingId =
  Esq.findOne' $ do
    rentalBooking <- from $ table @RentalBookingT
    where_ $ rentalBooking ^. RentalBookingBookingId ==. val (toKey bookingId)
    return rentalBooking
