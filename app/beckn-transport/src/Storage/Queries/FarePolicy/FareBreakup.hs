{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareBreakup where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import Storage.Tabular.FarePolicy.FareBreakup

create :: FareBreakup -> SqlDB ()
create = Esq.create

findAllByRideBookingId :: (MonadThrow m, Log m, Transactionable m) => Id RideBooking -> m [FareBreakup]
findAllByRideBookingId rideBookingId =
  findAll $ do
    fareBreakup <- from $ table @FareBreakupT
    where_ $ fareBreakup ^. FareBreakupRideBookingId ==. val (toKey rideBookingId)
    return fareBreakup
