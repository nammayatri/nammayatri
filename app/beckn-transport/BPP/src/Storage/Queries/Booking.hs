module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Booking.Type as Booking
import Domain.Types.Organization
import Domain.Types.RiderDetails (RiderDetails)
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation as Loc
import Storage.Tabular.Booking.OneWayBooking as OneWayBooking
import Storage.Tabular.Booking.RentalBooking as RentalBooking

create :: Booking -> SqlDB ()
create booking = do
  Esq.withFullEntity booking $ \(bookingT, fromLocT, bookingDetailsT) -> do
    -- order of creating make sense
    case bookingDetailsT of
      Booking.OneWayDetailsT (toLocT, oneWayBookingT) -> do
        Esq.create' fromLocT
        Esq.create' toLocT
        Esq.create' bookingT
        Esq.create' oneWayBookingT
      Booking.RentalDetailsT rentalBookingT -> do
        Esq.create' fromLocT
        Esq.create' bookingT
        Esq.create' rentalBookingT

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

updateRiderName :: Id Booking -> Text -> SqlDB ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderName =. val (Just riderName),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

fullBookingTable ::
  From
    ( Table BookingT
        :& Table Loc.BookingLocationT
        :& MbTable OneWayBooking.OneWayBookingT
        :& MbTable Loc.BookingLocationT
        :& MbTable RentalBooking.RentalBookingT
    )
fullBookingTable =
  table @BookingT
    `innerJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(booking :& loc1) ->
                   booking ^. BookingFromLocationId ==. loc1 ^. Loc.BookingLocationTId
               )
    `leftJoin` table @OneWayBooking.OneWayBookingT
      `Esq.on` ( \(booking :& _ :& mbOneWayBooking) ->
                   just (booking ^. BookingTId) ==. mbOneWayBooking ?. OneWayBooking.OneWayBookingBookingId
               )
    `leftJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(_ :& _ :& mbOneWayBooking :& mbLoc2) ->
                   mbOneWayBooking ?. OneWayBookingToLocationId ==. mbLoc2 ?. Loc.BookingLocationTId
               )
    `leftJoin` table @RentalBooking.RentalBookingT
      `Esq.on` ( \(booking :& _ :& _ :& _ :& mbRentalBooking) ->
                   just (booking ^. BookingTId) ==. mbRentalBooking ?. RentalBooking.RentalBookingBookingId
               )

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' $ do
    (booking :& fromLoc :& mbOneWayBooking :& mbToLoc :& mbRentalBooking) <- from fullBookingTable
    where_ $ booking ^. BookingTId ==. val (toKey bookingId)
    pure (booking, fromLoc, mbOneWayBooking, mbToLoc, mbRentalBooking)
  join <$> mapM buildFullBooking mbFullBookingT

findAllByOrg :: Transactionable m => Id Organization -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByOrg orgId mbLimit mbOffset mbIsOnlyActive mbBookingStatus = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
      isJustBookingStatus = isJust mbBookingStatus
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbOneWayBooking :& mbToLoc :& mbRentalBooking) <- from fullBookingTable
    where_ $
      booking ^. BookingProviderId ==. val (toKey orgId)
        &&. not_ (booking ^. BookingStatus `in_` valList [Booking.CONFIRMED, Booking.AWAITING_REASSIGNMENT])
        &&. whenTrue_ isOnlyActive (not_ $ booking ^. BookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
        &&. whenTrue_ isJustBookingStatus (booking ^. BookingStatus `in_` valList [fromJust mbBookingStatus])
    orderBy [desc $ booking ^. BookingCreatedAt]
    limit limitVal
    offset offsetVal
    pure (booking, fromLoc, mbOneWayBooking, mbToLoc, mbRentalBooking)
  catMaybes <$> mapM buildFullBooking fullBookingsT

increaseReallocationsCounter :: Id Booking -> SqlDB ()
increaseReallocationsCounter rbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingReallocationsCount +=. val 1,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)
