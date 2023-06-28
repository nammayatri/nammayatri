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

import qualified Data.List as List
import Data.Ord
import Domain.Types.Booking
import Domain.Types.Booking as DRB
import Domain.Types.FarePolicy.FareProductType
import Domain.Types.Location
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Domain.Types.Quote
import qualified Domain.Types.RentalSlab as DRentalSlab
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Queries.LocationMapping as QLocationMapping
import Storage.Queries.RentalSlab as QRentalSlab
import Storage.Queries.Ride as QRide
import Storage.Queries.TripTerms as QTripTerms
import Storage.Tabular.Booking
import qualified Storage.Tabular.Booking as RB

-- we already created TripTerms and RentalSlab when created Quote

bookingToBookingTableConverter :: Booking -> BookingTable
bookingToBookingTableConverter Booking {..} = case bookingDetails of
  DRB.OneWayDetails oneWayDetails -> do
    BookingTable
      { id = id.getId,
        bppBookingId = getId <$> bppBookingId,
        fareProductType = ONE_WAY,
        distance = Just $ oneWayDetails.distance,
        otpCode = Nothing,
        tripTermsId = (.id) <$> tripTerms,
        rentalSlabId = Nothing,
        ..
      }
  DRB.RentalDetails rentalSlab ->
    BookingTable
      { id = id.getId,
        bppBookingId = getId <$> bppBookingId,
        fareProductType = RENTAL,
        distance = Nothing,
        otpCode = Nothing,
        tripTermsId = (.id) <$> tripTerms,
        rentalSlabId = Just rentalSlab.id,
        ..
      }
  DRB.DriverOfferDetails driverOfferDetails -> do
    BookingTable
      { id = id.getId,
        bppBookingId = getId <$> bppBookingId,
        fareProductType = ONE_WAY,
        distance = Just $ driverOfferDetails.distance,
        otpCode = Nothing,
        tripTermsId = (.id) <$> tripTerms,
        rentalSlabId = Nothing,
        ..
      }
  DRB.OneWaySpecialZoneDetails oneWaySpZoneDetails -> do
    BookingTable
      { id = id.getId,
        bppBookingId = getId <$> bppBookingId,
        fareProductType = ONE_WAY,
        distance = Just $ oneWaySpZoneDetails.distance,
        otpCode = Nothing,
        tripTermsId = (.id) <$> tripTerms,
        rentalSlabId = Nothing,
        ..
      }

create :: Booking -> SqlDB ()
create booking = do
  let tablex = bookingToBookingTableConverter booking
  createBTable tablex

createBTable :: BookingTable -> SqlDB ()
createBTable = Esq.create

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateBPPBookingId :: Id Booking -> Id BPPBooking -> SqlDB ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateOtpCodeBookingId :: Id Booking -> Text -> SqlDB ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingOtpCode =. val (Just otp)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

findLatestByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
findLatestByRiderIdAndStatus riderId statusList =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey riderId)
        &&. booking ^. RB.BookingStatus `in_` valList statusList
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    limit 1
    pure $ booking ^. RB.BookingStatus

compressBooking :: [Booking] -> Maybe Booking
compressBooking list =
  if null list
    then Nothing
    else do
      let Booking {..} = head list
      let toLoc =
            concatMap
              ( \booking -> case booking.bookingDetails of
                  DRB.OneWayDetails owbd -> owbd.toLocation
                  DRB.RentalDetails _ -> []
                  DRB.DriverOfferDetails owbd -> owbd.toLocation
                  DRB.OneWaySpecialZoneDetails owszbd -> owszbd.toLocation
              )
              list

      let finalDetails = case bookingDetails of
            DRB.OneWayDetails OneWayBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }
            DRB.RentalDetails rs -> DRB.RentalDetails rs
            DRB.DriverOfferDetails OneWayBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }
            DRB.OneWaySpecialZoneDetails OneWaySpecialZoneBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }

      Just
        Booking
          { bookingDetails = finalDetails,
            ..
          }

findCountByRideIdAndStatus :: Transactionable m => Id Person -> BookingStatus -> m Int
findCountByRideIdAndStatus personId status = do
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @BookingT
      where_ $
        messageReport ^. BookingRiderId ==. val (toKey personId)
          &&. messageReport ^. BookingStatus ==. val status
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findAllByRiderIdAndRide :: (MonadFlow m, Transactionable m) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbStatus = do
  findByRiderIdList <- findByRiderId personId
  filteredBookingByRideId <- filterM checkIfValid findByRiderIdList
  let limit' = fromIntegral $ fromMaybe 10 mbLimit
  let offset' = fromIntegral $ fromMaybe 0 mbOffset
  let bookingList = take limit' (drop offset' filteredBookingByRideId)
  let b = sortBookingsDescending bookingList
  pure b
  where
    checkIfValid booking = do
      b <- findAllByRBId booking.id
      let isOnlyActive = Just True == mbOnlyActive
      let isOtpPresent = case booking.bookingDetails of
            DRB.OneWayDetails _ -> False
            DRB.RentalDetails _ -> False
            DRB.DriverOfferDetails _ -> False
            DRB.OneWaySpecialZoneDetails owszbd -> isJust owszbd.otpCode
      let completedAndCancelledstatus = case mbStatus of
            Just status -> status /= DRB.COMPLETED && status /= DRB.CANCELLED
            Nothing -> True
      let result = not (null b) || (null b && isOtpPresent) && (isOnlyActive || completedAndCancelledstatus)
      pure result

sortBookingsDescending :: [Booking] -> [Booking]
sortBookingsDescending = List.sortOn (Down . (.createdAt))

updatePaymentInfo :: Id Booking -> Money -> Maybe Money -> Money -> SqlDB ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingEstimatedFare =. val (realToFrac estimatedFare),
        RB.BookingDiscount =. val (realToFrac <$> discount),
        RB.BookingEstimatedTotalFare =. val (realToFrac estimatedTotalFare)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

findAssignedByRiderId :: (Transactionable m, MonadFlow m) => Id Person -> m (Maybe Booking)
findAssignedByRiderId personId = do
  mbBookingTable <- findBAssignedByRiderId personId
  case mbBookingTable of
    Just bookingTable -> bookingTableToBookingConverter bookingTable
    Nothing -> return Nothing

findBAssignedByRiderId :: Transactionable m => Id Person -> m (Maybe BookingTable)
findBAssignedByRiderId personId = findOne $ do
  booking <- from $ table @BookingT
  where_ $
    booking ^. RB.BookingRiderId ==. val (toKey personId)
      &&. booking ^. RB.BookingStatus ==. val TRIP_ASSIGNED
  return booking

findAllByRiderId :: (Transactionable m, MonadFlow m) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = do
  bookingTables <- findBAllByRiderId personId mbLimit mbOffset mbOnlyActive
  catMaybes <$> mapM bookingTableToBookingConverter bookingTables

findBAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [BookingTable]
findBAllByRiderId personId mbLimit mbOffset mbOnlyActive = findAll $ do
  booking <- from $ table @BookingT
  where_ $
    booking ^. RB.BookingRiderId ==. val (toKey personId)
      &&. whenTrue_ (Just True == mbOnlyActive) (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
  limit $ fromIntegral $ fromMaybe 10 mbLimit
  offset $ fromIntegral $ fromMaybe 0 mbOffset
  orderBy [desc $ booking ^. RB.BookingCreatedAt]
  return booking

findAllByBPPBookingId :: (Transactionable m, MonadFlow m) => Id BPPBooking -> m [Booking]
findAllByBPPBookingId bppRbId = do
  bookingTables <- findAllTableByBPPBookingId bppRbId
  catMaybes <$> mapM bookingTableToBookingConverter bookingTables

findByBPPBookingId :: (Transactionable m, MonadFlow m) => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId bppRbId = do
  mbBookingTable <- findBookingTableByBPPBookingId bppRbId
  case mbBookingTable of
    Just bookingTable -> bookingTableToBookingConverter bookingTable
    Nothing -> return Nothing

findByIdAndMerchantId :: (Transactionable m, MonadFlow m) => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId bookingId merchantId = do
  mbBookingTable <- findBookingTableByIdAndMerchantId bookingId merchantId
  case mbBookingTable of
    Just bookingTable -> bookingTableToBookingConverter bookingTable
    Nothing -> return Nothing

findByRiderIdAndStatus :: (MonadFlow m, Transactionable m) => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus personId statusList = do
  bookingTables <- findBookingTableByRiderIdAndStatus personId statusList
  catMaybes <$> mapM bookingTableToBookingConverter bookingTables

findByRiderId :: (MonadFlow m, Transactionable m) => Id Person -> m [Booking]
findByRiderId personId = do
  bookingTables <- findBookingTableByRiderId personId
  catMaybes <$> mapM bookingTableToBookingConverter bookingTables

findBookingTableByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m [BookingTable]
findBookingTableByRiderIdAndStatus personId statusList = findAll $ do
  bookingT <- from $ table @BookingT
  where_ $
    bookingT ^. RB.BookingRiderId ==. val (toKey personId)
      &&. bookingT ^. RB.BookingStatus `in_` valList statusList
  return bookingT

findBookingTableByRiderId :: Transactionable m => Id Person -> m [BookingTable]
findBookingTableByRiderId personId = findAll $ do
  bookingT <- from $ table @BookingT
  where_ $
    bookingT ^. RB.BookingRiderId ==. val (toKey personId)
  return bookingT

findBookingTableByIdAndMerchantId :: Transactionable m => Id Booking -> Id Merchant -> m (Maybe BookingTable)
findBookingTableByIdAndMerchantId bookingId merchantId = findOne $ do
  bookingT <- from $ table @BookingT
  where_ $
    bookingT ^. BookingId ==. val (getId bookingId) &&. bookingT ^. RB.BookingMerchantId ==. val (toKey merchantId)
  return bookingT

findAllByPersonIdLimitOffset ::
  (Transactionable m, MonadFlow m) =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [Booking]
findAllByPersonIdLimitOffset personId mlimit moffset = do
  bookingTables <- findAllBookingTableByByPersonIdLimitOffset personId mlimit moffset
  catMaybes <$> mapM bookingTableToBookingConverter bookingTables

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond' =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus ==. val NEW &&. upcoming6HrsCond')
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

findAssignedByQuoteId :: (MonadFlow m, Transactionable m) => Id Quote -> m (Maybe Booking)
findAssignedByQuoteId quoteId = do
  mbBookingTable <- findBookingTableAssignedByQuoteId quoteId
  case mbBookingTable of
    Just bookingTable -> bookingTableToBookingConverter bookingTable
    Nothing -> return Nothing

findBookingTableAssignedByQuoteId :: Transactionable m => Id Quote -> m (Maybe BookingTable)
findBookingTableAssignedByQuoteId quoteId = do
  findOne $ do
    bookingT <- from $ table @BookingT
    where_ $ bookingT ^. RB.BookingQuoteId ==. val (Just $ toKey quoteId)
    return bookingT

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

findAllBookingTableByByPersonIdLimitOffset :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> m [BookingTable]
findAllBookingTableByByPersonIdLimitOffset personId mlimit moffset = do
  findAll $ do
    bookingT <- from $ table @BookingT
    where_ $
      bookingT ^. BookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    return bookingT

findBookingTableByBPPBookingId :: Transactionable m => Id BPPBooking -> m (Maybe BookingTable)
findBookingTableByBPPBookingId bppRbId = do
  findOne $ do
    bookingT <- from $ table @BookingT
    where_ $ bookingT ^. RB.BookingBppBookingId ==. val (Just $ getId bppRbId)
    return bookingT

findAllTableByBPPBookingId :: Transactionable m => Id BPPBooking -> m [BookingTable]
findAllTableByBPPBookingId bppBookingId = do
  findAll $ do
    bookingT <- from $ table @BookingT
    where_ $
      bookingT ^. BookingBppBookingId ==. val (Just $ getId bppBookingId)
    return bookingT

buildBookingDetails :: (MonadFlow m) => FareProductType -> Maybe HighPrecMeters -> [Location] -> Maybe DRentalSlab.RentalSlab -> Maybe Text -> m BookingDetails
buildBookingDetails fareProductType distance toLocationList rentalSlab otpCode = case fareProductType of
  ONE_WAY -> buildOneWayDetails distance toLocationList
  RENTAL -> do
    rentalSlab' <- rentalSlab & fromMaybeM (InternalError "rental slab is missing from rental booking")
    return $ DRB.RentalDetails rentalSlab'
  DRIVER_OFFER -> buildOneWayDetails distance toLocationList
  ONE_WAY_SPECIAL_ZONE -> do
    distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
    return $
      DRB.OneWaySpecialZoneDetails
        DRB.OneWaySpecialZoneBookingDetails
          { distance = distance',
            toLocation = toLocationList,
            ..
          }

buildOneWayDetails :: (MonadFlow m) => Maybe HighPrecMeters -> [Location] -> m BookingDetails
buildOneWayDetails distance toLocationList = do
  distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
  return $
    DRB.OneWayDetails
      DRB.OneWayBookingDetails
        { distance = distance',
          toLocation = toLocationList
        }

findById :: (Transactionable m, MonadFlow m) => Id Booking -> m (Maybe Booking)
findById bookingId = do
  mbBookingTable <- getBookingTableByBookingId bookingId
  case mbBookingTable of
    Just bookingTable -> bookingTableToBookingConverter bookingTable
    Nothing -> return Nothing

bookingTableToBookingConverter :: (Transactionable m, MonadFlow m) => BookingTable -> m (Maybe Booking)
bookingTableToBookingConverter BookingTable {..} = do
  locationMappings <- QLocationMapping.findByTagId id
  let toLocation = map (.location) (filter (\locationMapping -> locationMapping.order /= 0) locationMappings)
  fromLocation <- sourceLocationFinder locationMappings & fromMaybeM (InternalError "from location is missing")
  tripTerms <- case tripTermsId of
    Just tid -> QTripTerms.findById tid
    Nothing -> return Nothing
  rentalSlab <- case rentalSlabId of
    Just rid -> QRentalSlab.findById rid
    Nothing -> return Nothing
  finalBookingDetails <- buildBookingDetails fareProductType distance toLocation rentalSlab otpCode
  return $
    Just
      Booking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          bookingDetails = finalBookingDetails,
          ..
        }
