module Storage.Queries.BookingCancellationReason where

import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
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

upsert :: BookingCancellationReason -> SqlDB ()
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
