{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking where

import Domain.Types.Booking.Type as Booking
import Domain.Types.Merchant
import Domain.Types.RiderDetails (RiderDetails)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking as Booking
import Storage.Tabular.Booking.BookingLocation as Loc
import Storage.Tabular.Booking.OneWayBooking as OneWayBooking
import Storage.Tabular.Booking.RentalBooking as RentalBooking

create :: Booking -> SqlDB m ()
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

updateStatus :: Id Booking -> BookingStatus -> SqlDB m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val rbStatus,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderId :: Id Booking -> Id RiderDetails -> SqlDB m ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingRiderId =. val (Just $ toKey riderId),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

updateRiderName :: Id Booking -> Text -> SqlDB m ()
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

findById :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Booking)
findById bookingId proxy = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' @m @ma $ do
    (booking :& fromLoc :& mbOneWayBooking :& mbToLoc :& mbRentalBooking) <- from fullBookingTable
    where_ $ booking ^. BookingTId ==. val (toKey bookingId)
    pure (booking, fromLoc, mbOneWayBooking, mbToLoc, mbRentalBooking)
  join <$> mapM (`buildFullBooking` proxy) mbFullBookingT

findAllByMerchant :: forall m ma. Transactionable ma m => Id Merchant -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> Proxy ma -> m [Booking]
findAllByMerchant merchantId mbLimit mbOffset mbIsOnlyActive mbBookingStatus proxy = Esq.buildDType $ do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  fullBookingsT <- Esq.findAll' @m @ma $ do
    (booking :& fromLoc :& mbOneWayBooking :& mbToLoc :& mbRentalBooking) <- from fullBookingTable
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. not_ (booking ^. BookingStatus `in_` valList [Booking.CONFIRMED, Booking.AWAITING_REASSIGNMENT])
        &&. whenTrue_ isOnlyActive (not_ $ booking ^. BookingStatus `in_` valList [Booking.COMPLETED, Booking.CANCELLED])
        &&. whenJust_ mbBookingStatus (\status -> booking ^. BookingStatus ==. val status)
    orderBy [desc $ booking ^. BookingCreatedAt]
    limit limitVal
    offset offsetVal
    pure (booking, fromLoc, mbOneWayBooking, mbToLoc, mbRentalBooking)
  catMaybes <$> mapM (`buildFullBooking` proxy) fullBookingsT

increaseReallocationsCounter :: Id Booking -> SqlDB m ()
increaseReallocationsCounter rbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingReallocationsCount +=. val 1,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey rbId)

findStuckBookings :: forall m ma. Transactionable ma m => Id Merchant -> [Id Booking] -> UTCTime -> Proxy ma -> m [Id Booking]
findStuckBookings merchantId bookingIds now _ = do
  Esq.findAll @m @ma $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

cancelBookings :: [Id Booking] -> UTCTime -> SqlDB m ()
cancelBookings bookingIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val CANCELLED,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)
