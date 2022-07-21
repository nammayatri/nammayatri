module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.Time
import Domain.Types.Booking
import Domain.Types.RiderDetails (RiderDetails)
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import qualified Storage.Tabular.FareParameters as Fare

create :: Booking -> SqlDB ()
create dsReq = Esq.runTransaction $
  withFullEntity dsReq $ \(sReq, fromLoc, toLoc, fareParams) -> do
    Esq.create' fareParams
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' sReq

baseBookingQuery ::
  From
    ( ( ( SqlExpr (Entity BookingT)
            :& SqlExpr (Entity BookingLocationT)
        )
          :& SqlExpr (Entity BookingLocationT)
      )
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseBookingQuery =
  table @BookingT
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& loc1) -> rb ^. BookingFromLocationId ==. loc1 ^. BookingLocationTId)
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& _ :& loc2) -> rb ^. BookingToLocationId ==. loc2 ^. BookingLocationTId)
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& _ :& _ :& farePars) ->
                   rb ^. BookingFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = buildDType $
  fmap (fmap extractSolidType) $
    Esq.findOne' $ do
      (rb :& bFromLoc :& bToLoc :& farePars) <-
        from baseBookingQuery
      where_ $ rb ^. BookingTId ==. val (toKey bookingId)
      pure (rb, bFromLoc, bToLoc, farePars)

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val rbStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderId :: Id Booking -> Id RiderDetails -> SqlDB ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderId =. val (Just $ toKey riderId),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)
