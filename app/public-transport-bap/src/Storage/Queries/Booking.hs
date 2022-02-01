module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Booking
import Domain.Quote
import GHC.Int
import Storage.Tabular.Booking
import Tools.Auth

findById :: EsqDBFlow m r => Id Booking -> m (Maybe Booking)
findById bookingId =
  runTransaction . findOne' $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingId ==. val (getId bookingId)
    return booking

findByQuoteId :: EsqDBFlow m r => Id Quote -> m (Maybe Booking)
findByQuoteId quoteId =
  runTransaction . findOne' $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingQuoteId ==. val (toKey quoteId)
    return booking

create :: Booking -> SqlDB ()
create = create'

update :: Booking -> SqlDB ()
update parkingBooking = do
  now <- getCurrentTime
  update' $ \tbl -> do
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
  update' $ \tbl -> do
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
  update' $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val newStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId booking.id)

findAllByRequestorId :: EsqDBFlow m r => PersonId -> Integer -> Integer -> m [Booking]
findAllByRequestorId personId limitInt offSetInt = do
  let limit_ :: Int64 = fromInteger limitInt
      offset_ :: Int64 = fromInteger offSetInt
  runTransaction . findAll' $ do
    publicTransportSearch <- from $ table @BookingT
    where_ $ publicTransportSearch ^. BookingRequestorId ==. val (getId personId)
    limit limit_
    offset offset_
    return publicTransportSearch
