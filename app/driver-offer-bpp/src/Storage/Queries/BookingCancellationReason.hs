module Storage.Queries.BookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
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
