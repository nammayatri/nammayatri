{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking where

import Domain.Types.Booking
import Domain.Types.Quote
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.Booking
import Tools.Auth

findById :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Booking)
findById bookingId _ =
  Esq.findOne @m @ma $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingId ==. val (getId bookingId)
    return booking

findByQuoteId :: forall m ma. Transactionable ma m => Id Quote -> Proxy ma -> m (Maybe Booking)
findByQuoteId quoteId _ =
  Esq.findOne @m @ma $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingQuoteId ==. val (toKey quoteId)
    return booking

create :: Booking -> SqlDB m ()
create = Esq.create

update :: Booking -> SqlDB m ()
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

updateStatusAndBppOrderId :: Booking -> SqlDB m ()
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

updateStatus :: Booking -> BookingStatus -> SqlDB m ()
updateStatus booking newStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val newStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingId ==. val (getId booking.id)

findAllByRequestorId :: forall m ma. Transactionable ma m => PersonId -> Integer -> Integer -> Maybe BookingStatus -> Proxy ma -> m [Booking]
findAllByRequestorId personId limitInt offSetInt mbBookingStatus _ = do
  let limit_ :: Int64 = fromInteger limitInt
      offset_ :: Int64 = fromInteger offSetInt
  Esq.findAll @m @ma $ do
    transportStationSearch <- from $ table @BookingT
    where_ $
      transportStationSearch ^. BookingRequestorId ==. val (getId personId)
        &&. whenJust_ mbBookingStatus (\status -> transportStationSearch ^. BookingStatus ==. val status)
    limit limit_
    offset offset_
    return transportStationSearch
