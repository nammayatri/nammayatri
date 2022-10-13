module Storage.Queries.BookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create

findByRideBookingId ::
  Transactionable m =>
  Id Booking ->
  m (Maybe BookingCancellationReason)
findByRideBookingId rideBookingId =
  Esq.findOne $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ rideBookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey rideBookingId)
    return rideBookingCancellationReason
