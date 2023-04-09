{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Domain.Types.Booking
import Domain.Types.Merchant
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Id
import Kernel.Types.Time
import Storage.Tabular.Booking
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.DriverQuote as DriverQuote
import qualified Storage.Tabular.FareParameters as Fare

-- fareParams already created with driverQuote
create :: Booking -> SqlDB ()
create dBooking = Esq.runTransaction $
  withFullEntity dBooking $ \(booking, fromLoc, toLoc, _fareParams) -> do
    Esq.create' fromLoc
    Esq.create' toLoc
    Esq.create' booking

baseBookingTable ::
  From
    ( Table BookingT
        :& Table BookingLocationT
        :& Table BookingLocationT
        :& Table Fare.FareParametersT
    )
baseBookingTable =
  table @BookingT
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& loc1) -> rb ^. BookingFromLocationId ==. loc1 ^. BookingLocationTId)
    `innerJoin` table @BookingLocationT `Esq.on` (\(rb :& _ :& loc2) -> rb ^. BookingToLocationId ==. loc2 ^. BookingLocationTId)
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& _ :& _ :& farePars) ->
                   rb ^. BookingFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

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
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond)
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

-- queries fetching only one entity should avoid join for performance reason

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = Esq.buildDType . runMaybeT $ do
  booking <- findByIdM @BookingT (toKey bookingId)
  fetchFullBookingM booking

findBookingBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = Esq.buildDType . runMaybeT $ do
  booking <- Esq.findOneM $ do
    booking <- from $ table @BookingT
    let otpExpiryCondition =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.MINUTE 30] >=. val now
    where_ $
      booking ^. BookingSpecialZoneOtpCode ==. val (Just otpCode)
        &&. (booking ^. BookingStatus ==. val NEW &&. otpExpiryCondition)
        &&. booking ^. BookingProviderId ==. val (toKey merchantId)
    pure booking
  fetchFullBookingM booking

findBySearchReq :: (Transactionable m) => Id DSR.SearchRequest -> m (Maybe Booking)
findBySearchReq searchReqId = Esq.buildDType . runMaybeT $ do
  driverQuote <- Esq.findOneM $ do
    driverQuote <- from $ table @DriverQuoteT
    where_ $ driverQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
    pure driverQuote
  booking <- Esq.findOneM $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. BookingQuoteId ==. val (driverQuote & DriverQuote.id)
    pure booking
  fetchFullBookingM booking

-- internal queries for building domain types

fetchFullBookingM ::
  Transactionable m =>
  BookingT ->
  MaybeT (DTypeBuilder m) (SolidType FullBookingT)
fetchFullBookingM booking@BookingT {..} = do
  fromLocation <- Esq.findByIdM @BookingLocationT fromLocationId
  toLocation <- Esq.findByIdM @BookingLocationT toLocationId
  fareParams <- Esq.findByIdM @Fare.FareParametersT fareParametersId
  return $ extractSolidType @Booking (booking, fromLocation, toLocation, fareParams)
