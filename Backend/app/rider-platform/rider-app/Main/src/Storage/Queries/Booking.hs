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
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import Storage.Tabular.Booking
import qualified Storage.Tabular.Booking as RB
import qualified Storage.Tabular.Booking.BookingLocation as Loc
import qualified Storage.Tabular.RentalSlab as RentalSlab
import qualified Storage.Tabular.Ride as R
import qualified Storage.Tabular.TripTerms as TripTerms

-- we already created TripTerms and RentalSlab when created Quote
create :: Booking -> SqlDB m ()
create booking =
  Esq.withFullEntity booking $ \(bookingT, fromLocT, _mbTripTermsT, bookingDetailsT) -> do
    Esq.create' fromLocT
    case bookingDetailsT of
      OneWayDetailsT toLocT -> Esq.create' toLocT
      RentalDetailsT _rentalSlabT -> pure ()
      DriverOfferDetailsT toLocT -> Esq.create' toLocT
    Esq.create' bookingT

updateStatus :: Id Booking -> BookingStatus -> SqlDB m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateBPPBookingId :: Id Booking -> Id BPPBooking -> SqlDB m ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

fullBookingTable ::
  From
    ( Table RB.BookingT
        :& Table Loc.BookingLocationT
        :& MbTable Loc.BookingLocationT
        :& MbTable TripTerms.TripTermsT
        :& MbTable RentalSlab.RentalSlabT
    )
fullBookingTable =
  table @BookingT
    `innerJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(s :& loc1) ->
                   s ^. RB.BookingFromLocationId ==. loc1 ^. Loc.BookingLocationTId
               )
    `leftJoin` table @Loc.BookingLocationT
      `Esq.on` ( \(s :& _ :& mbLoc2) ->
                   s ^. RB.BookingToLocationId ==. mbLoc2 ?. Loc.BookingLocationTId
               )
    `leftJoin` table @TripTerms.TripTermsT
      `Esq.on` ( \(s :& _ :& _ :& mbTripTerms) ->
                   s ^. RB.BookingTripTermsId ==. mbTripTerms ?. TripTerms.TripTermsTId
               )
    `leftJoin` table @RentalSlab.RentalSlabT
      `Esq.on` ( \(s :& _ :& _ :& _ :& mbRentalSlab) ->
                   s ^. RB.BookingRentalSlabId ==. mbRentalSlab ?. RentalSlab.RentalSlabTId
               )

findById :: forall m ma. Transactionable ma m => Id Booking -> Proxy ma -> m (Maybe Booking)
findById bookingId proxy = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $ booking ^. RB.BookingTId ==. val (toKey bookingId)
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  join <$> mapM (`buildFullBooking` proxy) mbFullBookingT

findByBPPBookingId :: forall m ma. Transactionable ma m => Id BPPBooking -> Proxy ma -> m (Maybe Booking)
findByBPPBookingId bppRbId proxy = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $ booking ^. RB.BookingBppBookingId ==. val (Just $ getId bppRbId)
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  join <$> mapM (`buildFullBooking` proxy) mbFullBookingT

findByIdAndMerchantId :: forall m ma. Transactionable ma m => Id Booking -> Id Merchant -> Proxy ma -> m (Maybe Booking)
findByIdAndMerchantId bookingId merchantId proxy = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. RB.BookingId ==. val bookingId.getId
        &&. booking ^. RB.BookingMerchantId ==. val (toKey merchantId)
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  join <$> mapM (`buildFullBooking` proxy) mbFullBookingT

findAllByRiderId :: forall m ma. Transactionable ma m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Proxy ma -> m [Booking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive proxy = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM (`buildFullBooking` proxy) fullBookingsT

findByRiderIdAndStatus :: forall m ma. Transactionable ma m => Id Person -> [BookingStatus] -> Proxy ma -> m [Booking]
findByRiderIdAndStatus personId statusList proxy = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. booking ^. RB.BookingStatus `in_` valList statusList
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM (`buildFullBooking` proxy) fullBookingsT

findAllByRiderIdAndRide :: forall m ma. Transactionable ma m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> Proxy ma -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus proxy = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab :& _) <-
      from $
        fullBookingTable `innerJoin` table @R.RideT
          `Esq.on` (\(booking :& _ :& _ :& _ :& _ :& ride) -> booking ^. RB.BookingTId ==. ride ^. R.RideBookingId)
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. ( whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
                &&. whenJust_ mbBookingStatus (\status -> booking ^. RB.BookingStatus ==. val status)
            )
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM (`buildFullBooking` proxy) fullBookingsT

updatePaymentInfo :: Id Booking -> Money -> Maybe Money -> Money -> SqlDB m ()
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

findAllByPersonIdLimitOffset ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  Proxy ma ->
  m [Booking]
findAllByPersonIdLimitOffset personId mlimit moffset proxy = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' @m @ma $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM (`buildFullBooking` proxy) fullBookingsT

findStuckBookings :: forall m ma. Transactionable ma m => Id Merchant -> [Id Booking] -> UTCTime -> Proxy ma -> m [Id Booking]
findStuckBookings merchantId bookingIds now _ = do
  Esq.findAll @m @ma $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
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
