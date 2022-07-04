module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking
import Domain.Types.Quote
import Storage.Tabular.Booking
import Tools.Auth

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingId ==. val (getId bookingId)
    return booking

findByQuoteId :: Transactionable m => Id Quote -> m (Maybe Booking)
findByQuoteId quoteId =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingQuoteId ==. val (toKey quoteId)
    return booking

create :: Booking -> SqlDB ()
create = Esq.create

update :: Booking -> SqlDB ()
update parkingBooking = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val parkingBooking.status,
        BookingTicketId =. val parkingBooking.ticketId,
        BookingTicketCreatedAt =. val parkingBooking.ticketCreatedAt,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId parkingBooking.id)

updateStatusAndBppOrderId :: Booking -> SqlDB ()
updateStatusAndBppOrderId parkingBooking = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val parkingBooking.status,
        BookingTicketId =. val parkingBooking.ticketId,
        BookingTicketCreatedAt =. val parkingBooking.ticketCreatedAt,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId parkingBooking.id)

updateStatus :: Booking -> BookingStatus -> SqlDB ()
updateStatus booking newStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val newStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId booking.id)

findAllByRequestorId :: Transactionable m => PersonId -> Integer -> Integer -> m [Booking]
findAllByRequestorId personId limitInt offSetInt = do
  let limit_ :: Int64 = fromInteger limitInt
      offset_ :: Int64 = fromInteger offSetInt
  Esq.findAll $ do
    transportStationSearch <- from $ table @BookingT
    where_ $ transportStationSearch ^. BookingRequestorId ==. val (getId personId)
    limit limit_
    offset offset_
    return transportStationSearch
