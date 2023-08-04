{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking.Internal where

import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import Storage.Tabular.Booking

getBookingInfo ::
  Transactionable m =>
  [Id DP.Person] ->
  m [Booking.Booking]
getBookingInfo driverIds = buildDType $ do
  res <-
    Esq.findAll' $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. BookingQuoteId `in_` valList (driverIds <&> (.getId))
          &&. booking ^. BookingStatus ==. val Booking.TRIP_ASSIGNED
      return booking
  catMaybes <$> mapM buildFullBooking res
