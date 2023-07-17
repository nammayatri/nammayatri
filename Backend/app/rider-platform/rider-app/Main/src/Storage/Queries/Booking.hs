{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Booking where

import Control.Applicative
import Data.Time (addUTCTime)
import Domain.Types.Booking.Type as Domain
import qualified Domain.Types.Booking.Type as DRB
import Domain.Types.Estimate (Estimate)
import Domain.Types.FarePolicy.FareProductType as DFF
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Error
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findAllWithOptionsKV, findOneWithKV, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import Storage.Queries.RentalSlab as QueryRS
import qualified Storage.Queries.TripTerms as QTT
import Storage.Tabular.Booking
import qualified Storage.Tabular.Booking as RB
import qualified Storage.Tabular.Booking.BookingLocation as Loc
import qualified Storage.Tabular.DriverOffer as DrOff
import qualified Storage.Tabular.Quote as Quote
import qualified Storage.Tabular.RentalSlab as RentalSlab
import qualified Storage.Tabular.Ride as R
import qualified Storage.Tabular.TripTerms as TripTerms

-- we already created TripTerms and RentalSlab when created Quote
-- create :: L.MonadFlow m => Booking -> m (MeshResult ())
-- create :: Booking -> SqlDB ()
-- create booking =
--   Esq.withFullEntity booking $ \(bookingT, fromLocT, _mbTripTermsT, bookingDetailsT) -> do
--     Esq.create' fromLocT
--     case bookingDetailsT of
--       OneWayDetailsT toLocT -> Esq.create' toLocT
--       RentalDetailsT _rentalSlabT -> pure ()
--       DriverOfferDetailsT toLocT -> Esq.create' toLocT
--       OneWaySpecialZoneDetailsT toLocT -> Esq.create' toLocT
--     Esq.create' bookingT

createBooking :: (L.MonadFlow m, Log m) => Booking -> m ()
createBooking = createWithKV

create :: (L.MonadFlow m, Log m) => Booking -> m ()
create dBooking = do
  _ <- QBBL.create (dBooking.fromLocation)
  _ <- case dBooking.bookingDetails of
    OneWayDetails toLoc -> void $ QBBL.create toLoc.toLocation
    RentalDetails _ -> pure ()
    DriverOfferDetails toLoc -> void $ QBBL.create toLoc.toLocation
    OneWaySpecialZoneDetails toLoc -> void $ QBBL.create toLoc.toLocation
  void $ createBooking dBooking

-- updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
-- updateStatus rbId rbStatus = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RB.BookingUpdatedAt =. val now,
--         RB.BookingStatus =. val rbStatus
--       ]
--     where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamB.status rbStatus,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

-- updateBPPBookingId :: Id Booking -> Id BPPBooking -> SqlDB ()
-- updateBPPBookingId rbId bppRbId = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RB.BookingUpdatedAt =. val now,
--         RB.BookingBppBookingId =. val (Just $ getId bppRbId)
--       ]
--     where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateBPPBookingId :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> Id BPPBooking -> m ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamB.bppBookingId (Just $ getId bppRbId),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

-- updateOtpCodeBookingId :: Id Booking -> Text -> SqlDB ()
-- updateOtpCodeBookingId rbId otp = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RB.BookingUpdatedAt =. val now,
--         RB.BookingOtpCode =. val (Just otp)
--       ]
--     where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateOtpCodeBookingId :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> Text -> m ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamB.otpCode (Just otp),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

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

-- findLatestByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
-- findLatestByRiderIdAndStatus riderId statusList =
--   Esq.findOne $ do
--     booking <- from $ table @BookingT
--     where_ $
--       booking ^. RB.BookingRiderId ==. val (toKey riderId)
--         &&. booking ^. RB.BookingStatus `in_` valList statusList
--     orderBy [desc $ booking ^. RB.BookingCreatedAt]
--     limit 1
--     pure $ booking ^. RB.BookingStatus

findLatestByRiderIdAndStatus :: (L.MonadFlow m, Log m) => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
findLatestByRiderIdAndStatus (Id bookingId) bookingStatus =
  do
    let options = [Se.And [Se.Is BeamB.id $ Se.Eq bookingId, Se.Is BeamB.status $ Se.In bookingStatus]]
        sortBy = Se.Desc BeamB.createdAt
        limit' = Just 1
    findAllWithOptionsKV options sortBy limit' Nothing
    <&> listToMaybe
    <&> (Domain.status <$>)

-- findById :: Transactionable m => Id Booking -> m (Maybe Booking)
-- findById bookingId = Esq.buildDType $ do
--   mbFullBookingT <- Esq.findOne' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $ booking ^. RB.BookingTId ==. val (toKey bookingId)
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   join <$> mapM buildFullBooking mbFullBookingT

findById :: (L.MonadFlow m, Log m) => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

-- findByBPPBookingId :: Transactionable m => Id BPPBooking -> m (Maybe Booking)
-- findByBPPBookingId bppRbId = Esq.buildDType $ do
--   mbFullBookingT <- Esq.findOne' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $ booking ^. RB.BookingBppBookingId ==. val (Just $ getId bppRbId)
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   join <$> mapM buildFullBooking mbFullBookingT

findByBPPBookingId :: (L.MonadFlow m, Log m) => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId (Id bppRbId) = findOneWithKV [Se.Is BeamB.bppBookingId $ Se.Eq $ Just bppRbId]

-- findByIdAndMerchantId :: Transactionable m => Id Booking -> Id Merchant -> m (Maybe Booking)
-- findByIdAndMerchantId bookingId merchantId = Esq.buildDType $ do
--   mbFullBookingT <- Esq.findOne' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $
--       booking ^. RB.BookingId ==. val bookingId.getId
--         &&. booking ^. RB.BookingMerchantId ==. val (toKey merchantId)
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   join <$> mapM buildFullBooking mbFullBookingT

findByIdAndMerchantId :: (L.MonadFlow m, Log m) => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId (Id bookingId) (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamB.id $ Se.Eq bookingId, Se.Is BeamB.merchantId $ Se.Eq merchantId]]

-- findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
-- findAllByRiderId personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
--   let isOnlyActive = Just True == mbOnlyActive
--   fullBookingsT <- Esq.findAll' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $
--       booking ^. RB.BookingRiderId ==. val (toKey personId)
--         &&. whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
--     limit $ fromIntegral $ fromMaybe 10 mbLimit
--     offset $ fromIntegral $ fromMaybe 0 mbOffset
--     orderBy [desc $ booking ^. RB.BookingCreatedAt]
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   catMaybes <$> mapM buildFullBooking fullBookingsT

findAllByRiderId :: (L.MonadFlow m, Log m) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId (Id personId) mbLimit mbOffset mbOnlyActive = do
  let limit' = fmap fromIntegral $ mbLimit <|> Just 10
      offset' = fmap fromIntegral $ mbOffset <|> Just 0
  findAllWithOptionsKV [Se.And ([Se.Is BeamB.riderId $ Se.Eq personId] <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED] | mbOnlyActive == Just True]))] (Se.Desc BeamB.createdAt) limit' offset'

-- findCountByRideIdAndStatus :: Transactionable m => Id Person -> BookingStatus -> m Int
-- findCountByRideIdAndStatus personId status = do
--   mkCount <$> do
--     Esq.findAll $ do
--       messageReport <- from $ table @BookingT
--       where_ $
--         messageReport ^. BookingRiderId ==. val (toKey personId)
--           &&. messageReport ^. BookingStatus ==. val status
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

findCountByRideIdAndStatus :: (L.MonadFlow m, Log m) => Id Person -> BookingStatus -> m Int
findCountByRideIdAndStatus (Id personId) status = findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq status]] <&> length

-- findCountByRideIdStatusAndTime :: Transactionable m => Id Person -> BookingStatus -> UTCTime -> UTCTime -> m Int
-- findCountByRideIdStatusAndTime personId status startTime endTime = do
--   mkCount <$> do
--     Esq.findAll $ do
--       booking <- from $ table @BookingT
--       where_ $
--         booking ^. BookingRiderId ==. val (toKey personId)
--           &&. booking ^. BookingStatus ==. val status
--           &&. (booking ^. BookingCreatedAt >=. val startTime &&. booking ^. BookingCreatedAt <. val endTime)
--       return (countRows :: SqlExpr (Esq.Value Int))
--   where
--     mkCount [counter] = counter
--     mkCount _ = 0

findCountByRideIdStatusAndTime :: (L.MonadFlow m, Log m) => Id Person -> BookingStatus -> UTCTime -> UTCTime -> m Int
findCountByRideIdStatusAndTime (Id personId) status startTime endTime = findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq status, Se.Is BeamB.createdAt $ Se.GreaterThanOrEq startTime, Se.Is BeamB.createdAt $ Se.LessThan endTime]] <&> length

-- findByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m [Booking]
-- findByRiderIdAndStatus personId statusList = Esq.buildDType $ do
--   fullBookingsT <- Esq.findAll' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $
--       booking ^. RB.BookingRiderId ==. val (toKey personId)
--         &&. booking ^. RB.BookingStatus `in_` valList statusList
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   catMaybes <$> mapM buildFullBooking fullBookingsT

findByRiderIdAndStatus :: (L.MonadFlow m, Log m) => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus (Id personId) statusList = findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.In statusList]]

-- findAssignedByRiderId :: Transactionable m => Id Person -> m (Maybe Booking)
-- findAssignedByRiderId personId = Esq.buildDType $ do
--   fullBookingsT <- Esq.findOne' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $
--       booking ^. RB.BookingRiderId ==. val (toKey personId)
--         &&. booking ^. RB.BookingStatus ==. val TRIP_ASSIGNED
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   join <$> mapM buildFullBooking fullBookingsT

findAssignedByRiderId :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe Booking)
findAssignedByRiderId (Id personId) = findOneWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq TRIP_ASSIGNED]]

findBookingIdAssignedByEstimateId :: Transactionable m => Id Estimate -> m (Maybe (Id Booking))
findBookingIdAssignedByEstimateId estimateId =
  Esq.findOne $ do
    (booking :& _ :& driverOffer) <-
      from $
        table @BookingT
          `innerJoin` table @Quote.QuoteT
            `Esq.on` ( \(rb :& quote) ->
                         rb ^. RB.BookingQuoteId ==. Esq.just (quote ^. Quote.QuoteTId)
                     )
          `innerJoin` table @DrOff.DriverOfferT
            `Esq.on` ( \(_ :& quote :& driverOffer) ->
                         quote ^. Quote.QuoteDriverOfferId ==. Esq.just (driverOffer ^. DrOff.DriverOfferTId)
                     )
    where_ $
      driverOffer ^. DrOff.DriverOfferEstimateId ==. val (toKey estimateId)
        &&. booking ^. RB.BookingStatus ==. val TRIP_ASSIGNED
    pure (booking ^. BookingTId)

findAllByRiderIdAndRide :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' $ do
    (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab :& mbRide) <-
      from $
        fullBookingTable `leftJoin` table @R.RideT
          `Esq.on` (\(booking :& _ :& _ :& _ :& _ :& mbRide) -> just (booking ^. RB.BookingTId) ==. mbRide ?. R.RideBookingId)
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. ( whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
                &&. whenJust_ mbBookingStatus (\status -> booking ^. RB.BookingStatus ==. val status)
                &&. ( not_ (Esq.isNothing (mbRide ?. R.RideTId))
                        ||. (Esq.isNothing (mbRide ?. R.RideTId) &&. not_ (Esq.isNothing (booking ^. RB.BookingOtpCode)))
                    )
            )
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
  catMaybes <$> mapM buildFullBooking fullBookingsT

-- updatePaymentInfo :: Id Booking -> Money -> Maybe Money -> Money -> Maybe Text -> SqlDB ()
-- updatePaymentInfo rbId estimatedFare discount estimatedTotalFare mbPaymentUrl = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RB.BookingUpdatedAt =. val now,
--         RB.BookingEstimatedFare =. val (realToFrac estimatedFare),
--         RB.BookingDiscount =. val (realToFrac <$> discount),
--         RB.BookingEstimatedTotalFare =. val (realToFrac estimatedTotalFare),
--         RB.BookingPaymentUrl =. val mbPaymentUrl
--       ]
--     where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updatePaymentInfo :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> Money -> Maybe Money -> Money -> Maybe Text -> m ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare mbPaymentUrl = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamB.estimatedFare (realToFrac estimatedFare),
      Se.Set BeamB.discount (realToFrac <$> discount),
      Se.Set BeamB.estimatedTotalFare (realToFrac estimatedTotalFare),
      Se.Set BeamB.paymentUrl mbPaymentUrl,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

-- updatePaymentUrl :: Id Booking -> Text -> SqlDB ()
-- updatePaymentUrl bookingId paymentUrl = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ RB.BookingPaymentUrl =. val (Just paymentUrl),
--         RB.BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. RB.BookingId ==. val (getId bookingId)

updatePaymentUrl :: (L.MonadFlow m, MonadTime m, Log m) => Id Booking -> Text -> m ()
updatePaymentUrl bookingId paymentUrl = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamB.paymentUrl (Just paymentUrl),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

-- findAllByPersonIdLimitOffset ::
--   Transactionable m =>
--   Id Person ->
--   Maybe Integer ->
--   Maybe Integer ->
--   m [Booking]
-- findAllByPersonIdLimitOffset personId mlimit moffset = Esq.buildDType $ do
--   fullBookingsT <- Esq.findAll' $ do
--     (booking :& fromLoc :& mbToLoc :& mbTripTerms :& mbRentalSlab) <- from fullBookingTable
--     where_ $
--       booking ^. RB.BookingRiderId ==. val (toKey personId)
--     limit $ fromIntegral $ fromMaybe 100 mlimit
--     offset $ fromIntegral $ fromMaybe 0 moffset
--     orderBy [desc $ booking ^. RB.BookingCreatedAt]
--     pure (booking, fromLoc, mbToLoc, mbTripTerms, mbRentalSlab)
--   catMaybes <$> mapM buildFullBooking fullBookingsT

findAllByPersonIdLimitOffset :: (L.MonadFlow m, Log m) => Id Person -> Maybe Integer -> Maybe Integer -> m [Booking]
findAllByPersonIdLimitOffset (Id personId) mlimit moffset = do
  let limit' = fmap fromIntegral $ mlimit <|> Just 100
      offset' = fmap fromIntegral $ moffset <|> Just 0
  findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq personId] (Se.Desc BeamB.createdAt) limit' offset'

-- findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
-- findStuckBookings merchantId bookingIds now = do
--   Esq.findAll $ do
--     booking <- from $ table @BookingT
--     let upcoming6HrsCond =
--           booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
--     where_ $
--       booking ^. BookingMerchantId ==. val (toKey merchantId)
--         &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
--         &&. (booking ^. BookingStatus `in_` valList [NEW, CONFIRMED, TRIP_ASSIGNED] &&. upcoming6HrsCond)
--     pure $ booking ^. BookingTId

findStuckBookings :: (L.MonadFlow m, MonadTime m, Log m) => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings (Id merchantId) bookingIds now =
  do
    let updatedTimestamp = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
    findAllWithKV
      [ Se.And
          [ Se.Is BeamB.merchantId $ Se.Eq merchantId,
            Se.Is BeamB.id (Se.In $ getId <$> bookingIds),
            Se.Is BeamB.status $ Se.Eq NEW,
            Se.Is BeamB.createdAt $ Se.LessThanOrEq updatedTimestamp
          ]
      ]
    <&> (Domain.id <$>)

-- cancelBookings :: [Id Booking] -> UTCTime -> SqlDB ()
-- cancelBookings bookingIds now = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ BookingStatus =. val CANCELLED,
--         BookingUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)

cancelBookings :: (L.MonadFlow m, MonadTime m, Log m) => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [ Se.Set BeamB.status CANCELLED,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

instance FromTType' BeamB.Booking Booking where
  fromTType' BeamB.BookingT {..} = do
    fl <- QBBL.findById (Id fromLocationId)
    tt <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    bookingDetails <- case fareProductType of
      DFF.ONE_WAY -> DRB.OneWayDetails <$> buildOneWayDetails toLocationId
      DFF.RENTAL -> do
        qd <- getRentalDetails rentalSlabId
        case qd of
          Nothing -> throwError (InternalError "No Rental Details present")
          Just a -> pure a
      DFF.DRIVER_OFFER -> DRB.OneWayDetails <$> buildOneWayDetails toLocationId
      DFF.ONE_WAY_SPECIAL_ZONE -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
    if isJust fl
      then
        pure $
          Just
            Booking
              { id = Id id,
                transactionId = transactionId,
                bppBookingId = Id <$> bppBookingId,
                quoteId = Id <$> quoteId,
                paymentMethodId = Id <$> paymentMethodId,
                paymentUrl = paymentUrl,
                status = status,
                providerId = providerId,
                providerUrl = pUrl,
                providerName = providerName,
                providerMobileNumber = providerMobileNumber,
                primaryExophone = primaryExophone,
                startTime = startTime,
                riderId = Id riderId,
                fromLocation = fromJust fl,
                estimatedFare = roundToIntegral estimatedFare,
                discount = roundToIntegral <$> discount,
                estimatedTotalFare = roundToIntegral estimatedTotalFare,
                vehicleVariant = vehicleVariant,
                bookingDetails = bookingDetails,
                tripTerms = tt,
                merchantId = Id merchantId,
                specialLocationTag = specialLocationTag,
                createdAt = createdAt,
                updatedAt = updatedAt
              }
      else pure Nothing
    where
      buildOneWayDetails _ = do
        toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          DRB.OneWayBookingDetails
            { toLocation = fromJust toLocation,
              distance = distance'
            }
      buildOneWaySpecialZoneDetails _ = do
        toLocation <- error ""
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          DRB.OneWaySpecialZoneBookingDetails
            { distance = distance',
              ..
            }
      getRentalDetails rentalSlabId' = do
        res <- maybe (pure Nothing) (QueryRS.findById . Id) rentalSlabId'
        case res of
          Just rentalSlab -> pure $ Just $ DRB.RentalDetails rentalSlab
          Nothing -> pure Nothing

instance ToTType' BeamB.Booking Booking where
  toTType' DRB.Booking {..} =
    let (fareProductType, toLocationId, distance, rentalSlabId, otpCode) = case bookingDetails of
          DRB.OneWayDetails details -> (DQuote.ONE_WAY, Just (getId details.toLocation.id), Just details.distance, Nothing, Nothing)
          DRB.RentalDetails rentalSlab -> (DQuote.RENTAL, Nothing, Nothing, Just . getId $ rentalSlab.id, Nothing)
          DRB.DriverOfferDetails details -> (DQuote.DRIVER_OFFER, Just (getId details.toLocation.id), Just details.distance, Nothing, Nothing)
          DRB.OneWaySpecialZoneDetails details -> (DQuote.ONE_WAY_SPECIAL_ZONE, Just (getId details.toLocation.id), Just details.distance, Nothing, details.otpCode)
     in BeamB.BookingT
          { BeamB.id = getId id,
            BeamB.transactionId = transactionId,
            BeamB.fareProductType = fareProductType,
            BeamB.bppBookingId = getId <$> bppBookingId,
            BeamB.quoteId = getId <$> quoteId,
            BeamB.paymentMethodId = getId <$> paymentMethodId,
            BeamB.paymentUrl = paymentUrl,
            BeamB.status = status,
            BeamB.providerId = providerId,
            BeamB.providerUrl = showBaseUrl providerUrl,
            BeamB.providerName = providerName,
            BeamB.providerMobileNumber = providerMobileNumber,
            BeamB.primaryExophone = primaryExophone,
            BeamB.startTime = startTime,
            BeamB.riderId = getId riderId,
            BeamB.fromLocationId = getId fromLocation.id,
            BeamB.toLocationId = toLocationId,
            BeamB.estimatedFare = realToFrac estimatedFare,
            BeamB.discount = realToFrac <$> discount,
            BeamB.estimatedTotalFare = realToFrac estimatedTotalFare,
            BeamB.otpCode = otpCode,
            BeamB.vehicleVariant = vehicleVariant,
            BeamB.distance = distance,
            BeamB.tripTermsId = getId <$> (tripTerms <&> (.id)),
            BeamB.rentalSlabId = rentalSlabId,
            BeamB.merchantId = getId merchantId,
            BeamB.specialLocationTag = specialLocationTag,
            BeamB.createdAt = createdAt,
            BeamB.updatedAt = updatedAt
          }
