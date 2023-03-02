{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB m ()
create = Esq.create

findByRideBookingId ::
  forall m ma.
  Transactionable ma m =>
  Id Booking ->
  Proxy ma ->
  m (Maybe BookingCancellationReason)
findByRideBookingId rideBookingId _ =
  Esq.findOne @m @ma $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ rideBookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey rideBookingId)
    return rideBookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB m ()
upsert cancellationReason@BookingCancellationReason {..} =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonSource =. val (cancellationReason.source),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonReasonStage =. val (cancellationReason.reasonStage),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]
