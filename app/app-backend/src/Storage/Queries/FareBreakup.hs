{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FareBreakup where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import Storage.Tabular.FarePolicy.FareBreakup

createMany :: [FareBreakup] -> SqlDB ()
createMany = Esq.createMany

findAllByBookingId :: (MonadThrow m, Log m, Transactionable m) => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId =
  findAll $ do
    fareBreakup <- from $ table @FareBreakupT
    where_ $ fareBreakup ^. FareBreakupBookingId ==. val (toKey bookingId)
    return fareBreakup
