{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FareBreakup where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.FareBreakup
import Domain.Types.RideBooking
import Storage.Tabular.FareBreakup

create :: FareBreakup -> SqlDB ()
create = Esq.create

findAllByRideBookingId :: (MonadThrow m, Log m, Transactionable m) => Id RideBooking -> m [FareBreakup]
findAllByRideBookingId rideBookingId =
  findAll $ do
    fareBreakup <- from $ table @FareBreakupT
    where_ $ fareBreakup ^. FareBreakupRideBookingId ==. val (toKey rideBookingId)
    return fareBreakup
