{-# LANGUAGE TypeApplications #-}

module Storage.Queries.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Domain.Types.RideBooking as DRB
import Storage.Queries.FullEntityBuilders (buildFullRideBooking)
import qualified Storage.Tabular.Ride as R
import qualified Storage.Tabular.RideBooking as RB
import Storage.Tabular.SearchRequest ()

-- we already created TripTerms and RentalSlab when created Quote
create :: RideBooking -> SqlDB ()
create booking =
  Esq.withFullEntity booking $ \(bookingT, _mbTripTermsT, _bookingDetailsT) -> do
    Esq.create' bookingT

updateStatus :: Id RideBooking -> RideBookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.RideBookingUpdatedAt =. val now,
        RB.RideBookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. RB.RideBookingId ==. val (getId rbId)

updateBPPBookingId :: Id RideBooking -> Id BPPRideBooking -> SqlDB ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.RideBookingUpdatedAt =. val now,
        RB.RideBookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. RB.RideBookingId ==. val (getId rbId)

findById :: Transactionable m => Id RideBooking -> m (Maybe RideBooking)
findById rideBookingId = Esq.buildDType $ do
  rideBookingT <- Esq.findById' rideBookingId
  join <$> mapM buildFullRideBooking rideBookingT

findByBPPBookingId :: Transactionable m => Id BPPRideBooking -> m (Maybe RideBooking)
findByBPPBookingId bppRbId = Esq.buildDType $ do
  bookingT <- Esq.findOne' $ do
    rideBooking <- from $ table @RB.RideBookingT
    where_ $ rideBooking ^. RB.RideBookingBppBookingId ==. val (Just $ getId bppRbId)
    return rideBooking
  join <$> mapM buildFullRideBooking bookingT

findByIdAndMerchantId :: Transactionable m => Id RideBooking -> Id Merchant -> m (Maybe RideBooking)
findByIdAndMerchantId rideBookingId merchantId = Esq.buildDType $ do
  bookingT <- Esq.findOne' $ do
    rideBooking <- from $ table @RB.RideBookingT
    where_ $
      rideBooking ^. RB.RideBookingId ==. val rideBookingId.getId
        &&. rideBooking ^. RB.RideBookingMerchantId ==. val (toKey merchantId)
    return rideBooking
  join <$> mapM buildFullRideBooking bookingT

findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  bookingT <- Esq.findAll' $ do
    rideBooking <- from $ table @RB.RideBookingT
    where_ $
      rideBooking ^. RB.RideBookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (rideBooking ^. RB.RideBookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ rideBooking ^. RB.RideBookingCreatedAt]
    return rideBooking
  catMaybes <$> mapM buildFullRideBooking bookingT

findByRiderIdAndStatus :: Transactionable m => Id Person -> RideBookingStatus -> m [RideBooking]
findByRiderIdAndStatus personId status = Esq.buildDType $ do
  bookingT <- Esq.findAll' $ do
    rideBooking <- from $ table @RB.RideBookingT
    where_ $
      rideBooking ^. RB.RideBookingRiderId ==. val (toKey personId)
        &&. rideBooking ^. RB.RideBookingStatus ==. val status
    return rideBooking
  catMaybes <$> mapM buildFullRideBooking bookingT

findAllByRiderIdAndRide :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [RideBooking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  bookingT <- Esq.findAll' $ do
    (rideBooking :& _) <-
      from $
        table @RB.RideBookingT `innerJoin` table @R.RideT
          `Esq.on` (\(rideBooking :& ride) -> rideBooking ^. RB.RideBookingTId ==. ride ^. R.RideBookingId)
    where_ $
      rideBooking ^. RB.RideBookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (rideBooking ^. RB.RideBookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ rideBooking ^. RB.RideBookingCreatedAt]
    return rideBooking
  catMaybes <$> mapM buildFullRideBooking bookingT

updatePaymentInfo :: Id RideBooking -> Amount -> Maybe Amount -> Amount -> SqlDB ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.RideBookingUpdatedAt =. val now,
        RB.RideBookingEstimatedFare =. val estimatedFare,
        RB.RideBookingDiscount =. val discount,
        RB.RideBookingEstimatedTotalFare =. val estimatedTotalFare
      ]
    where_ $ tbl ^. RB.RideBookingId ==. val (getId rbId)

findAllByPersonIdLimitOffset ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [RideBooking]
findAllByPersonIdLimitOffset personId mlimit moffset = Esq.buildDType $ do
  bookingT <- Esq.findAll' $ do
    rideBooking <- from $ table @RB.RideBookingT
    where_ $
      rideBooking ^. RB.RideBookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ rideBooking ^. RB.RideBookingCreatedAt]
    return rideBooking
  catMaybes <$> mapM buildFullRideBooking bookingT
