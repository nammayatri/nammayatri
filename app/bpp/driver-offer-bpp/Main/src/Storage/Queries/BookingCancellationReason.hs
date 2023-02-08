module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
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

findByRideId :: Transactionable m => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId rideId = Esq.findOne $ do
  bookingCancellationReason <- from $ table @BookingCancellationReasonT
  where_ $ bookingCancellationReason ^. BookingCancellationReasonRideId ==. (just . val . toKey $ rideId)
  return bookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB ()
upsert cancellationReason@BookingCancellationReason {..} =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]
