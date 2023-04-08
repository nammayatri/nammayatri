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

import Domain.Types.Booking as DRB
import Domain.Types.FarePolicy.FareProductType
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Domain.Types.Quote
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import Storage.Tabular.Booking
import qualified Storage.Tabular.Booking as TB
import qualified Storage.Tabular.Booking.BookingLocation as Loc
import qualified Storage.Tabular.RentalSlab as RentalSlab
import qualified Storage.Tabular.Ride as R
import qualified Storage.Tabular.TripTerms as TripTerms

-- we already created TripTerms and RentalSlab when created Quote
create :: Booking -> SqlDB ()
create booking =
  Esq.withFullEntity booking $ \(bookingT, fromLocT, _mbTripTermsT, bookingDetailsT) -> do
    Esq.create' fromLocT
    case bookingDetailsT of
      OneWayDetailsT toLocT -> Esq.create' toLocT
      RentalDetailsT _rentalSlabT -> pure ()
      DriverOfferDetailsT toLocT -> Esq.create' toLocT
      OneWaySpecialZoneDetailsT toLocT -> Esq.create' toLocT
    Esq.create' bookingT

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TB.BookingUpdatedAt =. val now,
        TB.BookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. TB.BookingId ==. val (getId rbId)

updateBPPBookingId :: Id Booking -> Id BPPBooking -> SqlDB ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TB.BookingUpdatedAt =. val now,
        TB.BookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. TB.BookingId ==. val (getId rbId)

updateOtpCodeBookingId :: Id Booking -> Text -> SqlDB ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TB.BookingUpdatedAt =. val now,
        TB.BookingOtpCode =. val (Just otp)
      ]
    where_ $ tbl ^. TB.BookingId ==. val (getId rbId)

fullBookingTable ::
  From
    ( Table TB.BookingT
        :& Table Loc.BookingLocationT
        :& MbTable Loc.BookingLocationT
        :& MbTable TripTerms.TripTermsT
        :& MbTable RentalSlab.RentalSlabT
    )
fullBookingTable =
  table @BookingT
    `innerJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(s :& loc1) ->
                   s ^. TB.BookingFromLocationId ==. loc1 ^. Loc.BookingLocationTId
               )
    `leftJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(s :& _ :& mbLoc2) ->
                   s ^. TB.BookingToLocationId ==. mbLoc2 ?. Loc.BookingLocationTId
               )
    `leftJoin` table @TripTerms.TripTermsT
      `Esq.on` ( \(s :& _ :& _ :& mbTripTerms) ->
                   s ^. TB.BookingTripTermsId ==. mbTripTerms ?. TripTerms.TripTermsTId
               )
    `leftJoin` table @RentalSlab.RentalSlabT
      `Esq.on` ( \(s :& _ :& _ :& _ :& mbRentalSlab) ->
                   s ^. TB.BookingRentalSlabId ==. mbRentalSlab ?. RentalSlab.RentalSlabTId
               )

findLatestByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
findLatestByRiderIdAndStatus riderId statusList =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. TB.BookingRiderId ==. val (toKey riderId)
        &&. booking ^. TB.BookingStatus `in_` valList statusList
    orderBy [desc $ booking ^. TB.BookingCreatedAt]
    limit 1
    pure $ booking ^. TB.BookingStatus

findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. TB.BookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (booking ^. TB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. TB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM buildFullBooking fullBookingsT

findByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus personId statusList = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. TB.BookingRiderId ==. val (toKey personId)
        &&. booking ^. TB.BookingStatus `in_` valList statusList
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM buildFullBooking fullBookingsT

findAllByRiderIdAndRide :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab :& mbRide) <-
      from $
        fullBookingTable `leftJoin` table @R.RideT
          `Esq.on` (\(booking :& _ :& _ :& _ :& _ :& mbRide) -> just (booking ^. TB.BookingTId) ==. mbRide ?. R.RideBookingId)
    where_ $
      booking ^. TB.BookingRiderId ==. val (toKey personId)
        &&. ( whenTrue_ isOnlyActive (not_ (booking ^. TB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
                &&. whenJust_ mbBookingStatus (\status -> booking ^. TB.BookingStatus ==. val status)
                &&. ( not_ (Esq.isNothing (mbRide ?. R.RideTId))
                        ||. (Esq.isNothing (mbRide ?. R.RideTId) &&. not_ (Esq.isNothing (booking ^. TB.BookingOtpCode)))
                    )
            )
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. TB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM buildFullBooking fullBookingsT

updatePaymentInfo :: Id Booking -> Money -> Maybe Money -> Money -> SqlDB ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TB.BookingUpdatedAt =. val now,
        TB.BookingEstimatedFare =. val (realToFrac estimatedFare),
        TB.BookingDiscount =. val (realToFrac <$> discount),
        TB.BookingEstimatedTotalFare =. val (realToFrac estimatedTotalFare)
      ]
    where_ $ tbl ^. TB.BookingId ==. val (getId rbId)

findAllByPersonIdLimitOffset ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [Booking]
findAllByPersonIdLimitOffset personId mlimit moffset = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. TB.BookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ booking ^. TB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM buildFullBooking fullBookingsT

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
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
  booking <- findByIdT @BookingT (toKey bookingId)
  (fromLocation, bookingDetails, mbTripTerms) <- findBookingEntitiesT booking
  return $ extractSolidType @Booking (booking, fromLocation, mbTripTerms, bookingDetails)

findByBPPBookingId :: Transactionable m => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId bppRbId = Esq.buildDType . runMaybeT $ do
  booking <- findByBPPBookingIdT
  (fromLocation, bookingDetails, mbTripTerms) <- findBookingEntitiesT booking
  return $ extractSolidType @Booking (booking, fromLocation, mbTripTerms, bookingDetails)
  where
    findByBPPBookingIdT = MaybeT . Esq.findOne' $ do
      booking <- from $ table @BookingT
      where_ $ booking ^. TB.BookingBppBookingId ==. val (Just $ getId bppRbId)
      pure booking

findByIdAndMerchantId :: Transactionable m => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId bookingId merchantId = Esq.buildDType . runMaybeT $ do
  booking <- findByIdAndMerchantIdT
  (fromLocation, bookingDetails, mbTripTerms) <- findBookingEntitiesT booking
  return $ extractSolidType @Booking (booking, fromLocation, mbTripTerms, bookingDetails)
  where
    findByIdAndMerchantIdT = MaybeT . Esq.findOne' $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. TB.BookingId ==. val bookingId.getId
          &&. booking ^. TB.BookingMerchantId ==. val (toKey merchantId)
      pure booking

findAssignedByRiderId :: Transactionable m => Id Person -> m (Maybe Booking)
findAssignedByRiderId personId = Esq.buildDType . runMaybeT $ do
  booking <- findAssignedByRiderIdT
  (fromLocation, bookingDetails, mbTripTerms) <- findBookingEntitiesT booking
  return $ extractSolidType @Booking (booking, fromLocation, mbTripTerms, bookingDetails)
  where
    findAssignedByRiderIdT = MaybeT . Esq.findOne' $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. TB.BookingRiderId ==. val (toKey personId)
          &&. booking ^. TB.BookingStatus ==. val TRIP_ASSIGNED
      pure booking

findAssignedByQuoteId :: Transactionable m => Id Quote -> m (Maybe Booking)
findAssignedByQuoteId quoteId = Esq.buildDType . runMaybeT $ do
  booking <- findAssignedByQuoteIdT
  (fromLocation, bookingDetails, mbTripTerms) <- findBookingEntitiesT booking
  return $ extractSolidType @Booking (booking, fromLocation, mbTripTerms, bookingDetails)
  where
    findAssignedByQuoteIdT = MaybeT . Esq.findOne' $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. TB.BookingQuoteId ==. val (Just $ toKey quoteId)
          &&. booking ^. TB.BookingStatus ==. val TRIP_ASSIGNED
      pure booking

-- internal queries for building domain types

findBookingEntitiesT ::
  Transactionable m =>
  BookingT ->
  MaybeT (DTypeBuilder m) (Loc.BookingLocationT, BookingDetailsT, Maybe TripTerms.TripTermsT)
findBookingEntitiesT booking = do
  fromLocation <- findByIdT @Loc.BookingLocationT (booking & TB.fromLocationId)
  bookingDetails <- case booking & TB.fareProductType of
    ONE_WAY -> do
      toLocationTId <- hoistMaybe (booking & TB.toLocationId)
      toLocation <- Esq.findByIdT @Loc.BookingLocationT toLocationTId
      pure $ OneWayDetailsT toLocation
    RENTAL -> do
      rentalSlabTId <- hoistMaybe (booking & TB.rentalSlabId)
      rentalSlab <- Esq.findByIdT @RentalSlab.RentalSlabT rentalSlabTId
      pure $ RentalDetailsT rentalSlab
    DRIVER_OFFER -> do
      toLocationTId <- hoistMaybe (booking & TB.toLocationId)
      toLocation <- Esq.findByIdT @Loc.BookingLocationT toLocationTId
      pure $ DriverOfferDetailsT toLocation
    ONE_WAY_SPECIAL_ZONE -> do
      toLocationTId <- hoistMaybe (booking & TB.toLocationId)
      toLocation <- Esq.findByIdT @Loc.BookingLocationT toLocationTId
      pure $ OneWaySpecialZoneDetailsT toLocation
  mbTripTerms <- forM (booking & TB.tripTermsId) $ Esq.findByIdT @TripTerms.TripTermsT
  return (fromLocation, bookingDetails, mbTripTerms)
