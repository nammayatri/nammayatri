{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Storage.Domain.Booking as Domain

findById :: (Esq.EsqDBFlow m r, HasLog r) => Id Domain.Booking -> m (Maybe Domain.Booking)
findById parkingSearchId =
  Esq.runTransaction . Esq.findOne' $ do
    parkingSearch <- Esq.from $ Esq.table @Domain.BookingT
    Esq.where_ $ parkingSearch Esq.^. Domain.BookingTId Esq.==. Esq.val (Domain.BookingTKey $ getId parkingSearchId)
    return parkingSearch

create :: Domain.Booking -> SqlDB Domain.Booking
create = Esq.createReturningEntity'

update :: Domain.Booking -> SqlDB ()
update parkingBooking = do
  now <- getCurrentTime
  Esq.update' $ \tbl -> do
    Esq.set
      tbl
      [ Domain.BookingTStatus Esq.=. Esq.val parkingBooking.status,
        Domain.BookingTTicketId Esq.=. Esq.val parkingBooking.ticketId,
        Domain.BookingTTicketCreatedAt Esq.=. Esq.val parkingBooking.ticketCreatedAt,
        Domain.BookingTUpdatedAt Esq.=. Esq.val now
      ]
    Esq.where_ $ tbl Esq.^. Domain.BookingTId Esq.==. Esq.val (toKey parkingBooking)
