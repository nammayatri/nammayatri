{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Booking where

import Control.Applicative
import Data.Ord
import qualified Database.Beam as B
import qualified Domain.Types.Booking.BookingLocation as DBBL
import Domain.Types.Booking.Type as Domain
import qualified Domain.Types.Booking.Type as DRB
import Domain.Types.Estimate (Estimate)
import Domain.Types.FarePolicy.FareProductType as DFF
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverOffer as BeamDO
import qualified Storage.Beam.Quote as BeamQ
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking.BookingLocation as QBBL
import qualified Storage.Queries.DriverOffer ()
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Quote ()
import qualified Storage.Queries.TripTerms as QTT

createBooking' :: MonadFlow m => Booking -> m ()
createBooking' = createWithKV

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
create dBooking = do
  _ <- whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create (dBooking.fromLocation)
  _ <- case dBooking.bookingDetails of
    OneWayDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    RentalDetails detail -> whenJust detail.stopLocation $ \stopLoc -> void $ whenNothingM_ (QL.findById stopLoc.id) $ do QL.create stopLoc
    DriverOfferDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    OneWaySpecialZoneDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
  void $ createBooking' dBooking

createBooking :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
createBooking booking = do
  fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  mbToLocationMap <- case booking.bookingDetails of
    DRB.OneWayDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.RentalDetails detail -> (\loc -> SLM.buildDropLocationMapping loc.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)) `mapM` detail.stopLocation
    DRB.DriverOfferDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.OneWaySpecialZoneDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)

  void $ QLM.create fromLocationMap
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create booking

updateStatus :: MonadFlow m => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.status rbStatus,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateBPPBookingId :: MonadFlow m => Id Booking -> Id BPPBooking -> m ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.bppBookingId (Just $ getId bppRbId),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateOtpCodeBookingId :: MonadFlow m => Id Booking -> Text -> m ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.otpCode (Just otp),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

findLatestByRiderIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
findLatestByRiderIdAndStatus (Id riderId) bookingStatusList =
  do
    let options = [Se.And [Se.Is BeamB.riderId $ Se.Eq riderId, Se.Is BeamB.status $ Se.In bookingStatusList]]
        sortBy = Se.Desc BeamB.createdAt
        limit' = Just 1
    findAllWithOptionsKV options sortBy limit' Nothing
    <&> listToMaybe
    <&> (Domain.status <$>)

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findByBPPBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId (Id bppRbId) = findOneWithKV [Se.Is BeamB.bppBookingId $ Se.Eq $ Just bppRbId]

findByIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId (Id bookingId) (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamB.id $ Se.Eq bookingId, Se.Is BeamB.merchantId $ Se.Eq merchantId]]

findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId (Id personId) mbLimit mbOffset mbOnlyActive = do
  let limit' = fmap fromIntegral $ mbLimit <|> Just 10
      offset' = fmap fromIntegral $ mbOffset <|> Just 0
  findAllWithOptionsKV [Se.And ([Se.Is BeamB.riderId $ Se.Eq personId] <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED] | mbOnlyActive == Just True]))] (Se.Desc BeamB.createdAt) limit' offset'

findCountByRiderIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> BookingStatus -> m Int
findCountByRiderIdAndStatus (Id personId) status = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\booking' -> (BeamB.riderId booking' B.==?. B.val_ personId) B.&&?. BeamB.status booking' B.==?. B.val_ status)
            do
              B.all_ (BeamCommon.booking BeamCommon.atlasDB)

  pure $ either (const 0) (\r -> if null r then 0 else head r) res

findCountByRideIdStatusAndTime :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> BookingStatus -> UTCTime -> UTCTime -> m Int
findCountByRideIdStatusAndTime (Id personId) status startTime endTime = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\booking' -> (BeamB.riderId booking' B.==?. B.val_ personId) B.&&?. BeamB.status booking' B.==?. B.val_ status B.&&?. B.sqlBool_ (BeamB.createdAt booking' B.>=. B.val_ startTime) B.&&?. B.sqlBool_ (BeamB.createdAt booking' B.<. B.val_ endTime))
            do
              B.all_ (BeamCommon.booking BeamCommon.atlasDB)

  pure $ either (const 0) (\r -> if null r then 0 else head r) res

findByRiderIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus (Id personId) statusList = findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.In statusList]]

findActiveBookingIdByRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe (Id Booking))
findActiveBookingIdByRiderId (Id personId) = do
  bookings <- findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.In activeBookingStatus]]
  return $ listToMaybe $ Domain.id <$> bookings

findAssignedByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Booking)
findAssignedByRiderId (Id personId) = findOneWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq TRIP_ASSIGNED]]

findBookingIdAssignedByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> m (Maybe (Id Booking))
findBookingIdAssignedByEstimateId (Id estimateId) = do
  driverOffer <- findAllWithKV [Se.Is BeamDO.estimateId $ Se.Eq estimateId]
  quote <- findAllWithKV [Se.Is BeamQ.driverOfferId $ Se.In $ map (\x -> Just (getId x.id)) driverOffer]
  booking <- findAllWithKV [Se.Is BeamB.quoteId $ Se.In $ map (\x -> Just (getId x.id)) quote, Se.Is BeamB.status $ Se.Eq TRIP_ASSIGNED]
  return $ listToMaybe $ Domain.id <$> booking

updatePaymentInfo :: MonadFlow m => Id Booking -> Money -> Maybe Money -> Money -> Maybe Text -> m ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare mbPaymentUrl = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.estimatedFare (realToFrac estimatedFare),
      Se.Set BeamB.discount (realToFrac <$> discount),
      Se.Set BeamB.estimatedTotalFare (realToFrac estimatedTotalFare),
      Se.Set BeamB.paymentUrl mbPaymentUrl,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updatePaymentUrl :: MonadFlow m => Id Booking -> Text -> m ()
updatePaymentUrl bookingId paymentUrl = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.paymentUrl (Just paymentUrl),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

-- THIS IS TEMPORARY UNTIL WE HAVE PROPER ADD STOP FEATURE
updateStop :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> Maybe DL.Location -> m ()
updateStop booking mbStopLoc = do
  now <- getCurrentTime
  -- whenJust mbStopLoc $ \stopLoc -> do
  --   void $ whenNothingM_ (QL.findById stopLoc.id) $ QL.create stopLoc
  -- locationMapping <- SLM.buildDropLocationMapping stopLoc.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  -- QLM.create locationMapping

  updateOneWithKV
    [ Se.Set BeamB.stopLocationId ((getId . (.id)) <$> mbStopLoc),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId booking.id)]

findAllByPersonIdLimitOffset :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> m [Booking]
findAllByPersonIdLimitOffset (Id personId) mlimit moffset = do
  let limit' = fmap fromIntegral $ mlimit <|> Just 100
      offset' = fmap fromIntegral $ moffset <|> Just 0
  findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq personId] (Se.Desc BeamB.createdAt) limit' offset'

findStuckBookings :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchant moCity bookingIds now =
  do
    let updatedTimestamp = addUTCTime (- (6 * 60 * 60) :: NominalDiffTime) now
    findAllWithDb
      [ Se.And
          [ Se.Is BeamB.merchantId $ Se.Eq merchant.id.getId,
            Se.Is BeamB.id (Se.In $ getId <$> bookingIds),
            Se.Is BeamB.status $ Se.In [NEW, CONFIRMED, TRIP_ASSIGNED],
            Se.Is BeamB.fareProductType $ Se.Not $ Se.Eq DQuote.RENTAL,
            Se.Is BeamB.createdAt $ Se.LessThanOrEq updatedTimestamp,
            Se.Or
              ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq $ Just (getId moCity.id)]
                  <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | moCity.city == merchant.defaultCity]
              )
          ]
      ]
    <&> (Domain.id <$>)

findAllCancelledBookingIdsByRider :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [Id Booking]
findAllCancelledBookingIdsByRider (Id riderId) =
  findAllWithDb
    [ Se.And
        [ Se.Is BeamB.riderId $ Se.Eq riderId,
          Se.Is BeamB.status $ Se.Eq CANCELLED
        ]
    ]
    <&> (Domain.id <$>)

cancelBookings :: MonadFlow m => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [ Se.Set BeamB.status CANCELLED,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

instance FromTType' BeamB.Booking Booking where
  fromTType' BeamB.BookingT {..} = do
    mappings <- QLM.findByEntityId id
    (fl, bookingDetails) <-
      if null mappings
        then do
          -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
          logInfo "Accessing Booking Location Table"
          pickupLoc <- upsertFromLocationAndMappingForOldData (Id <$> fromLocationId) id merchantId merchantOperatingCityId
          bookingDetails <- case fareProductType of
            DFF.ONE_WAY -> do
              upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
              DRB.OneWayDetails <$> buildOneWayDetails toLocationId
            DFF.RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            DFF.DRIVER_OFFER -> do
              upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
              DRB.OneWayDetails <$> buildOneWayDetails toLocationId
            DFF.ONE_WAY_SPECIAL_ZONE -> do
              upsertToLocationAndMappingForOldData toLocationId id merchantId merchantOperatingCityId
              DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocationId
          return (pickupLoc, bookingDetails)
        else do
          let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
              toLocationMappings = filter (\loc -> loc.order /= 0) mappings
          let toLoc = if null toLocationMappings then Nothing else Just $ maximumBy (comparing (.order)) toLocationMappings
              toLocId = (.locationId.getId) <$> toLoc

          fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
          fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)
          bookingDetails <- case fareProductType of
            DFF.ONE_WAY -> DRB.OneWayDetails <$> buildOneWayDetails toLocId
            DFF.RENTAL -> DRB.RentalDetails <$> buildRentalDetails stopLocationId
            DFF.DRIVER_OFFER -> DRB.OneWayDetails <$> buildOneWayDetails toLocId
            DFF.ONE_WAY_SPECIAL_ZONE -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails toLocId
          return (fl, bookingDetails)
    tt <- if isJust tripTermsId then QTT.findById'' (Id (fromJust tripTermsId)) else pure Nothing
    pUrl <- parseBaseUrl providerUrl
    merchantOperatingCityId' <- backfillMOCId merchantOperatingCityId
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
            fulfillmentId = fulfillmentId,
            driverId = driverId,
            itemId = itemId,
            providerMobileNumber = providerMobileNumber,
            primaryExophone = primaryExophone,
            startTime = startTime,
            riderId = Id riderId,
            fromLocation = fl,
            estimatedFare = roundToIntegral estimatedFare,
            discount = roundToIntegral <$> discount,
            estimatedTotalFare = roundToIntegral estimatedTotalFare,
            vehicleVariant = vehicleVariant,
            bookingDetails = bookingDetails,
            tripTerms = tt,
            merchantId = Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            specialLocationTag = specialLocationTag,
            createdAt = createdAt,
            updatedAt = updatedAt
          }
    where
      buildOneWayDetails toLocid = do
        toLocation <- maybe (pure Nothing) (QL.findById . Id) toLocid >>= fromMaybeM (InternalError "toLocation is null for one way booking")
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          DRB.OneWayBookingDetails
            { toLocation = toLocation,
              distance = distance'
            }
      buildOneWaySpecialZoneDetails toLocid = do
        toLocation <- maybe (pure Nothing) (QL.findById . Id) toLocid >>= fromMaybeM (InternalError "toLocation is null for one way special zone booking")
        distance' <- distance & fromMaybeM (InternalError "distance is null for one way booking")
        pure
          DRB.OneWaySpecialZoneBookingDetails
            { distance = distance',
              toLocation = toLocation,
              ..
            }
      buildRentalDetails mbStopLocationId = do
        mbStopLocation <- maybe (pure Nothing) (QL.findById . Id) mbStopLocationId
        pure
          DRB.RentalBookingDetails
            { stopLocation = mbStopLocation
            }

      backfillMOCId = \case
        Just mocId -> pure $ Id mocId
        Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

instance ToTType' BeamB.Booking Booking where
  toTType' DRB.Booking {..} =
    let (fareProductType, toLocationId, distance, stopLocationId, otpCode) = case bookingDetails of
          DRB.OneWayDetails details -> (DQuote.ONE_WAY, Just (getId details.toLocation.id), Just details.distance, Nothing, Nothing)
          DRB.RentalDetails rentalDetails -> (DQuote.RENTAL, Nothing, Nothing, (getId . (.id)) <$> rentalDetails.stopLocation, Nothing)
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
            BeamB.fulfillmentId = fulfillmentId,
            BeamB.driverId = driverId,
            BeamB.itemId = itemId,
            BeamB.fromLocationId = Just $ getId fromLocation.id,
            BeamB.toLocationId = toLocationId,
            BeamB.estimatedFare = realToFrac estimatedFare,
            BeamB.discount = realToFrac <$> discount,
            BeamB.estimatedTotalFare = realToFrac estimatedTotalFare,
            BeamB.otpCode = otpCode,
            BeamB.vehicleVariant = vehicleVariant,
            BeamB.distance = distance,
            BeamB.tripTermsId = getId <$> (tripTerms <&> (.id)),
            BeamB.stopLocationId = stopLocationId,
            BeamB.merchantId = getId merchantId,
            BeamB.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
            BeamB.specialLocationTag = specialLocationTag,
            BeamB.createdAt = createdAt,
            BeamB.updatedAt = updatedAt
          }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME

buildLocation :: MonadFlow m => DBBL.BookingLocation -> m DL.Location
buildLocation DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        ..
      }

upsertFromLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe (Id DBBL.BookingLocation) -> Text -> Text -> Maybe Text -> m DL.Location
upsertFromLocationAndMappingForOldData locationId bookingId merchantId merchantOperatingCityId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "From Location Id Not Found in Booking Table")
  pickupLoc <- maybe (throwError $ InternalError ("From Location Not Found in Booking Location Table for BookingId : " <> bookingId)) buildLocation loc
  fromLocationMapping <- SLM.buildPickUpLocationMapping pickupLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create pickupLoc >> QLM.create fromLocationMapping
  return pickupLoc

upsertToLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Text -> Text -> Maybe Text -> m ()
upsertToLocationAndMappingForOldData toLocationId bookingId merchantId merchantOperatingCityId = do
  toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId >>= fromMaybeM (InternalError "toLocation is null for one way booking")
  dropLoc <- buildLocation toLocation
  toLocationMapping <- SLM.buildDropLocationMapping dropLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create dropLoc >> QLM.create toLocationMapping
