module Storage.Queries.BookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import Domain.Types.Ride
import Storage.Tabular.BookingCancellationReason

create :: BookingCancellationReason -> SqlDB ()
create = Esq.create

findByRideId :: Transactionable m => Id Ride -> m (Maybe BookingCancellationReason)
findByRideId rideId = Esq.findOne $ do
  bookingCancellationReason <- from $ table @BookingCancellationReasonT
  where_ $ bookingCancellationReason ^. BookingCancellationReasonRideId ==. (just . val . toKey $ rideId)
  return bookingCancellationReason

findByRideBookingId ::
  Transactionable m =>
  Id Booking ->
  m (Maybe BookingCancellationReason)
findByRideBookingId rideBookingId =
  Esq.findOne $ do
    rideBookingCancellationReason <- from $ table @BookingCancellationReasonT
    where_ $ rideBookingCancellationReason ^. BookingCancellationReasonBookingId ==. val (toKey rideBookingId)
    return rideBookingCancellationReason

upsert :: BookingCancellationReason -> SqlDB ()
upsert cancellationReason@BookingCancellationReason {..} =
  Esq.upsert
    cancellationReason
    [ BookingCancellationReasonBookingId =. val (toKey cancellationReason.bookingId),
      BookingCancellationReasonRideId =. val (toKey <$> cancellationReason.rideId),
      BookingCancellationReasonSource =. val (cancellationReason.source),
      BookingCancellationReasonReasonCode =. val (toKey <$> cancellationReason.reasonCode),
      BookingCancellationReasonDriverId =. val (toKey <$> cancellationReason.driverId),
      BookingCancellationReasonAdditionalInfo =. val (cancellationReason.additionalInfo)
    ]
