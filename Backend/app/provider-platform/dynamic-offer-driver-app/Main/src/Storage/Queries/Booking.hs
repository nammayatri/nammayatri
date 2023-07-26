{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Booking where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.Booking
import Domain.Types.DriverQuote (DriverQuote)
import Domain.Types.Merchant
import qualified Domain.Types.Ride as DRide
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchTry as DST
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Id
import Kernel.Types.Time
import qualified Storage.Queries.DriverQuote as QDQuote
import Storage.Queries.FullEntityBuilders
import Storage.Tabular.Booking
import Storage.Tabular.DriverQuote as DriverQuote
import Storage.Tabular.Ride as Ride

-- fareParams already created with driverQuote
create :: Booking -> SqlDB ()
create dBooking =
  withFullEntity dBooking $ \(booking, fromLoc, toLoc, _fareParams) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' booking

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = buildDType $ do
  res <-
    Esq.findOne' $ do
      rb <- from $ table @BookingT
      where_ $ rb ^. BookingTId ==. val (toKey bookingId)
      pure rb
  join <$> mapM buildFullBooking res

findBySTId :: (Transactionable m) => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = buildDType $ do
  mbDriverQuoteT <- QDQuote.findDriverQuoteBySTId searchTryId
  let mbDriverQuoteId = Id . DriverQuote.id <$> mbDriverQuoteT
  mbBookingT <- (join <$>) $ mapM findBookingByDriverQuoteId' mbDriverQuoteId

  join <$> mapM buildFullBooking mbBookingT

findBookingByDriverQuoteId' :: Transactionable m => Id DriverQuote -> DTypeBuilder m (Maybe BookingT)
findBookingByDriverQuoteId' driverQuoteId = Esq.findOne' $ do
  booking <- from $ table @BookingT
  where_ $ booking ^. BookingQuoteId ==. val driverQuoteId.getId
  pure booking

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

updateSpecialZoneOtpCode :: Id Booking -> Text -> SqlDB ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingSpecialZoneOtpCode =. val (Just specialZoneOtpCode),
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId ==. val (toKey bookingId)

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus `in_` valList [NEW, TRIP_ASSIGNED] &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

findBookingBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    findById
    bookingId

findBookingIdBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe (Id Booking))
findBookingIdBySpecialZoneOTP merchantId otpCode now = do
  Esq.findOne $ do
    booking <- from $ table @BookingT
    let otpExpiryCondition =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.MINUTE 30] >=. val now
    where_ $
      booking ^. BookingSpecialZoneOtpCode ==. val (Just otpCode)
        &&. (booking ^. BookingStatus ==. val NEW &&. otpExpiryCondition)
        &&. booking ^. BookingProviderId ==. val (toKey merchantId)
    pure $ booking ^. BookingTId

cancelBookings :: [Id Booking] -> UTCTime -> SqlDB ()
cancelBookings bookingIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val CANCELLED,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)

findRideBookingsById :: Transactionable m => Id Merchant -> [Id Booking] -> m (HashMap.HashMap (Id Booking) (Booking, Maybe DRide.Ride))
findRideBookingsById merchantId bookingIds = do
  bookings <- findBookingsById merchantId bookingIds
  rides <- findRidesByBookingId (bookings <&> (.id))
  let tuple = map (\booking -> (booking.id, (booking, find (\ride -> ride.bookingId == booking.id) rides))) bookings
  pure $ HashMap.fromList tuple

findBookingsById :: Transactionable m => Id Merchant -> [Id Booking] -> m [Booking]
findBookingsById merchantId bookingIds = Esq.buildDType $ do
  bookingTs <- Esq.findAll' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. BookingProviderId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
    return booking
  catMaybes <$> forM bookingTs buildFullBooking

findRidesByBookingId :: Transactionable m => [Id Booking] -> m [DRide.Ride]
findRidesByBookingId bookingIds = Esq.findAll $ do
  ride <- from $ table @Ride.RideT
  where_ $
    ride ^. RideBookingId `in_` valList (toKey <$> bookingIds)
  return ride
