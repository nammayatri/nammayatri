module Storage.Queries.RecurringBooking where

import Data.Time.Calendar (Day)
import Domain.Types.RecurringBooking
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Tabular.Booking.BookingLocation as Loc
import Storage.Tabular.RecurringBooking

create :: RecurringBooking -> SqlDB ()
create recurringBooking =
  Esq.withFullEntity recurringBooking $ \(recurringBookingT, fromLocationT, toLocationT) -> do
    Esq.create' fromLocationT
    Esq.create' toLocationT
    Esq.create' recurringBookingT

fullRecurringBookingT ::
  From
    ( Table RecurringBookingT
        :& Table Loc.BookingLocationT
        :& Table Loc.BookingLocationT
    )
fullRecurringBookingT =
  table @RecurringBookingT
    `innerJoin` table @Loc.BookingLocationT
    `Esq.on` (\(rb :& l) -> rb ^. RecurringBookingFromLocationId ==. l ^. Loc.BookingLocationTId)
    `innerJoin` table @Loc.BookingLocationT
    `Esq.on` (\(rb :& _ :& l) -> rb ^. RecurringBookingToLocationId ==. l ^. Loc.BookingLocationTId)

findById :: Transactionable m => Id RecurringBooking -> m (Maybe RecurringBooking)
findById recurringBookingId = Esq.buildDType $ do
  mRes <- Esq.findOne' $ do
    (booking :& fromLocation :& toLocation) <- from fullRecurringBookingT
    where_ $ booking ^. RecurringBookingTId ==. val (toKey recurringBookingId)
    pure (booking, fromLocation, toLocation)
  pure $ extractSolidType @RecurringBooking <$> mRes

findAllActiveOnDate :: Transactionable m => Day -> m [RecurringBooking]
findAllActiveOnDate day = Esq.buildDType $ do
  res <- Esq.findAll' $ do
    (booking :& fromLocation :& toLocation) <- from fullRecurringBookingT
    where_ $
      booking ^. RecurringBookingStartDate >=. val day
        &&. (Esq.isNothing (booking ^. RecurringBookingEndDate) ||. booking ^. RecurringBookingEndDate <=. val (Just day))
    pure (booking, fromLocation, toLocation)
  pure $ extractSolidType @RecurringBooking <$> res

updateStatus :: Id RecurringBooking -> Status -> SqlDB ()
updateStatus recurringBookingId status =
  Esq.update $ \tbl -> do
    set
      tbl
      [ RecurringBookingStatus =. val status
      ]
    where_ $ tbl ^. RecurringBookingId ==. val (getId recurringBookingId)

updateEndDate :: Id RecurringBooking -> Maybe Day -> SqlDB ()
updateEndDate recurringBookingId mEndDate =
  Esq.update $ \tbl -> do
    set
      tbl
      [ RecurringBookingEndDate =. val mEndDate
      ]
    where_ $ tbl ^. RecurringBookingId ==. val (getId recurringBookingId)
