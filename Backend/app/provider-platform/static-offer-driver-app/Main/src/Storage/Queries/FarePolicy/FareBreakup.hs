{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.FarePolicy.FareBreakup where

import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.FarePolicy.FareBreakup

create :: FareBreakup -> SqlDB m ()
create = Esq.create

findAllByBookingId :: forall m ma. (MonadThrow m, Log m, Transactionable ma m) => Id Booking -> Proxy ma -> m [FareBreakup]
findAllByBookingId bookingId _ =
  findAll @m @ma $ do
    fareBreakup <- from $ table @FareBreakupT
    where_ $ fareBreakup ^. FareBreakupBookingId ==. val (toKey bookingId)
    return fareBreakup
