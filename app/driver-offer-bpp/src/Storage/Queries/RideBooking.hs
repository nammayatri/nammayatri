module Storage.Queries.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Time
import Domain.Types.RideBooking
import Storage.Tabular.RideBooking
import Storage.Tabular.RideBooking.BookingLocation

create :: RideBooking -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

baseRideBookingQuery ::
  From
    ( ( SqlExpr (Entity RideBookingT)
          :& SqlExpr (Entity BookingLocationT)
      )
        :& SqlExpr (Entity BookingLocationT)
    )
baseRideBookingQuery =
  table @RideBookingT
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& loc1) -> rb ^. RideBookingFromLocationId ==. loc1 ^. BookingLocationTId)
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& _ :& loc2) -> rb ^. RideBookingToLocationId ==. loc2 ^. BookingLocationTId)

findById :: Transactionable m => Id RideBooking -> m (Maybe RideBooking)
findById rideBookingId = buildDType $
  fmap (fmap extractSolidType) $
    Esq.findOne' $ do
      (rb :& bFromLoc :& bToLoc) <-
        from baseRideBookingQuery
      where_ $ rb ^. RideBookingTId ==. val (toKey rideBookingId)
      pure (rb, bFromLoc, bToLoc)

updateStatus :: Id RideBooking -> RideBookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RideBookingStatus =. val rbStatus,
        RideBookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. RideBookingTId ==. val (toKey rbId)
