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
import Domain.Types.DriverQuote (DriverQuote)
import Domain.Types.Location
import qualified Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.Merchant
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq hiding (findById, isNothing)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QFareParameters
import Storage.Queries.FullEntityBuilders
import qualified Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.Booking
import Storage.Tabular.DriverQuote as DriverQuote

bookingTableToBookingConverter :: (Transactionable m) => BookingTable -> m Booking
bookingTableToBookingConverter BookingTable {..} = do
  locationMappings <- QLocationMapping.findByTagId id.getId
  let toLocation = map (.location) (filter (\locationMapping -> locationMapping.order /= 0) locationMappings)
  fromLocation <- sourceLocationFinder locationMappings & fromMaybeM (InternalError "from location is missing")
  fareParams' <- QFareParameters.findById fareParametersId
  fareParams <- fareParams' & fromMaybeM (InternalError "fare params is missing")
  return
    Booking
      { id = Id id.getId,
        ..
      }

sourceLocationFinder :: [DLocationMapping.LocationMapping] -> Maybe Location
sourceLocationFinder locationMappings = do
  let orderZeroMappings = filter (\locationMapping -> locationMapping.order == 0) locationMappings
  if null orderZeroMappings
    then Nothing
    else do
      let source = head orderZeroMappings
      Just $ source.location

bookingToBookingTableConverter :: Booking -> BookingTable
bookingToBookingTableConverter Booking {..} =
  BookingTable
    { id = Id id.getId,
      fareParametersId = fareParams.id,
      ..
    }

create :: Booking -> SqlDB ()
create booking = do
  let tablex = bookingToBookingTableConverter booking
  createBTable tablex

createBTable :: BookingTable -> SqlDB ()
createBTable = Esq.create

findById :: (Transactionable m, MonadFlow m) => Id Booking -> m (Maybe Booking)
findById bookingId = do
  mbBookingTable <- getBookingTableByBookingId bookingId
  case mbBookingTable of
    Just bookingTable -> do
      booking <- bookingTableToBookingConverter bookingTable
      return $ Just booking
    Nothing -> return Nothing

getBookingTableByBookingId ::
  (Transactionable m) =>
  Id Booking ->
  m (Maybe BookingTable)
getBookingTableByBookingId bookingId = do
  findOne $ do
    bookingT <- from $ table @BookingT
    where_ $
      bookingT ^. BookingId ==. val (getId bookingId)
    return bookingT

findBySearchReq :: (Transactionable m, MonadFlow m) => Id DSR.SearchRequest -> m (Maybe Booking)
findBySearchReq req = do
  mbBookingTable <- findBookingTableBySearchReq req
  case mbBookingTable of
    Just bookingTable -> do
      booking <- bookingTableToBookingConverter bookingTable
      return $ Just booking
    Nothing -> return Nothing

findBookingTableBySearchReq :: (Transactionable m) => Id DSR.SearchRequest -> m (Maybe BookingTable)
findBookingTableBySearchReq searchReqId = buildDType $ do
  mbDriverQuoteT <- QDQuote.findDriverQuoteBySearchId searchReqId
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
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

findBookingBySpecialZoneOTP :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Text -> UTCTime -> m (Maybe Booking)
findBookingBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    findById
    bookingId

findBookingTableBySpecialZoneOTP :: Transactionable m => Id Merchant -> Text -> UTCTime -> m (Maybe BookingTable)
findBookingTableBySpecialZoneOTP merchantId otpCode now = do
  bookingId <- findBookingIdBySpecialZoneOTP merchantId otpCode now
  maybe
    (return Nothing)
    getBookingTableByBookingId
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
