{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareBreakup where

import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.FareBreakup

create :: FareBreakup -> SqlDB ()
create = Esq.create

findAllByBookingId :: (MonadThrow m, Log m, Transactionable m) => Id Booking -> m [FareBreakup]
findAllByBookingId bookingId =
  findAll $ do
    fareBreakup <- from $ table @FareBreakupT
    where_ $ fareBreakup ^. FareBreakupBookingId ==. val (toKey bookingId)
    return fareBreakup
