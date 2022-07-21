{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Booking as DRB
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import qualified Storage.Tabular.Booking as RB
import qualified Storage.Tabular.Ride as R
import Storage.Tabular.SearchRequest ()

-- we already created TripTerms and RentalSlab when created Quote
create :: Booking -> SqlDB ()
create booking =
  Esq.withFullEntity booking $ \(bookingT, _mbTripTermsT, _bookingDetailsT) -> do
    Esq.create' bookingT

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

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = Esq.buildDType $ do
  bookingT <- Esq.findById' bookingId
  join <$> mapM buildFullBooking bookingT

findByBPPBookingId :: Transactionable m => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId bppRbId = Esq.buildDType $ do
  bookingT <- Esq.findOne' $ do
    booking <- from $ table @RB.BookingT
    where_ $ booking ^. RB.BookingBppBookingId ==. val (Just $ getId bppRbId)
    return booking
  join <$> mapM buildFullBooking bookingT

findByIdAndMerchantId :: Transactionable m => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId bookingId merchantId = Esq.buildDType $ do
  bookingT <- Esq.findOne' $ do
    booking <- from $ table @RB.BookingT
    where_ $
      booking ^. RB.BookingId ==. val bookingId.getId
        &&. booking ^. RB.BookingMerchantId ==. val (toKey merchantId)
    return booking
  join <$> mapM buildFullBooking bookingT

findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  bookingT <- Esq.findAll' $ do
    booking <- from $ table @RB.BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    return booking
  catMaybes <$> mapM buildFullBooking bookingT

findByRiderIdAndStatus :: Transactionable m => Id Person -> BookingStatus -> m [Booking]
findByRiderIdAndStatus personId status = Esq.buildDType $ do
  bookingT <- Esq.findAll' $ do
    booking <- from $ table @RB.BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. booking ^. RB.BookingStatus ==. val status
    return booking
  catMaybes <$> mapM buildFullBooking bookingT

findAllByRiderIdAndRide :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  bookingT <- Esq.findAll' $ do
    (booking :& _) <-
      from $
        table @RB.BookingT `innerJoin` table @R.RideT
          `Esq.on` (\(booking :& ride) -> booking ^. RB.BookingTId ==. ride ^. R.RideBookingId)
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    return booking
  catMaybes <$> mapM buildFullBooking bookingT

updatePaymentInfo :: Id Booking -> Amount -> Maybe Amount -> Amount -> SqlDB ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingEstimatedFare =. val estimatedFare,
        RB.BookingDiscount =. val discount,
        RB.BookingEstimatedTotalFare =. val estimatedTotalFare
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

findAllByPersonIdLimitOffset ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [Booking]
findAllByPersonIdLimitOffset personId mlimit moffset = Esq.buildDType $ do
  bookingT <- Esq.findAll' $ do
    booking <- from $ table @RB.BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    return booking
  catMaybes <$> mapM buildFullBooking bookingT
