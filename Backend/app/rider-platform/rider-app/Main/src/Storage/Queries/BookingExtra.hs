module Storage.Queries.BookingExtra where

import Control.Applicative
import qualified Database.Beam as B
import Domain.Types
import Domain.Types.Booking as Domain
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingLocation as DBBL
import Domain.Types.Estimate (Estimate)
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
import qualified Storage.Queries.BookingLocation as QBBL
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.DriverOffer ()
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.Booking ()
import qualified Storage.Queries.Quote ()

createBooking' :: (MonadFlow m, EsqDBFlow m r) => Booking -> m ()
createBooking' = createWithKV

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
create dBooking = do
  _ <- whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create (dBooking.fromLocation)
  _ <- case dBooking.bookingDetails of
    OneWayDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    RentalDetails detail -> whenJust detail.stopLocation $ \stopLoc -> void $ whenNothingM_ (QL.findById stopLoc.id) $ do QL.create stopLoc
    DriverOfferDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    OneWaySpecialZoneDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    InterCityDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    AmbulanceDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
    DeliveryDetails detail -> void $ whenNothingM_ (QL.findById detail.toLocation.id) $ do QL.create detail.toLocation
    OneWayScheduledDetails toLoc -> void $ whenNothingM_ (QL.findById toLoc.toLocation.id) $ do QL.create toLoc.toLocation
  void $ createBooking' dBooking

createBooking :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
createBooking booking = do
  fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  mbToLocationMap <- case booking.bookingDetails of
    DRB.OneWayDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.RentalDetails detail -> (\loc -> SLM.buildDropLocationMapping loc.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)) `mapM` detail.stopLocation
    DRB.DriverOfferDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.OneWaySpecialZoneDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.InterCityDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.AmbulanceDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.DeliveryDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    DRB.OneWayScheduledDetails detail -> Just <$> SLM.buildDropLocationMapping detail.toLocation.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)

  void $ QLM.create fromLocationMap
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create booking

updateStatus :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.status rbStatus,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateBPPBookingId :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Id BPPBooking -> m ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.bppBookingId (Just $ getId bppRbId),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateOtpCodeBookingId :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Text -> m ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.otpCode (Just otp),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

findLatestSelfAndPartyBookingByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Booking)
findLatestSelfAndPartyBookingByRiderId (Id riderId) =
  do
    let options =
          [ Se.And
              [ Se.Is BeamB.riderId $ Se.Eq riderId,
                Se.Or
                  [ Se.And [Se.Is BeamB.status $ Se.In activeScheduledBookingStatus, Se.Is BeamB.isScheduled $ Se.Eq (Just True)],
                    Se.And [Se.Is BeamB.status $ Se.In activeBookingStatus, Se.Is BeamB.isScheduled $ Se.Not $ Se.Eq (Just True)]
                  ]
              ]
          ]
        sortBy' = Se.Desc BeamB.createdAt
        limit' = Just 1
    res <-
      findAllWithOptionsKV options sortBy' limit' Nothing
        <&> listToMaybe
    if (isNothing res)
      then do
        bookingParty <- QBPL.findOneActivePartyByRiderId (Id riderId)
        case bookingParty of
          Just bp -> findById bp.bookingId
          Nothing -> return Nothing
      else (return res)

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findByBPPBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId (Id bppRbId) = findOneWithKV [Se.Is BeamB.bppBookingId $ Se.Eq $ Just bppRbId]

findByTransactionId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe Booking)
findByTransactionId transactionId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamB.riderTransactionId $ Se.Eq transactionId
    ]
    (Just (Se.Desc BeamB.createdAt))
    <&> listToMaybe

findByIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId (Id bookingId) (Id merchantId) = findOneWithKV [Se.And [Se.Is BeamB.id $ Se.Eq bookingId, Se.Is BeamB.merchantId $ Se.Eq merchantId]]

-- findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
-- findAllByRiderId (Id personId) mbLimit mbOffset mbOnlyActive = do
--   let limit' = fmap fromIntegral $ mbLimit <|> Just 10
--       offset' = fmap fromIntegral $ mbOffset <|> Just 0
--   findAllWithOptionsKV [Se.And ([Se.Is BeamB.riderId $ Se.Eq personId] <> ([Se.Is BeamB.status $ Se.Not $ Se.In [DRB.COMPLETED, DRB.CANCELLED] | mbOnlyActive == Just True]))] (Se.Desc BeamB.createdAt) limit' offset'

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

findCountByRideIdStatusAndVehicleServiceTierType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> BookingStatus -> [ServiceTierType] -> m Int
findCountByRideIdStatusAndVehicleServiceTierType (Id personId) status vehicleServiceTierType =
  findAllWithKV
    [ Se.And
        [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq status, Se.Is BeamB.vehicleVariant $ Se.In vehicleServiceTierType]
    ]
    <&> length

findByRiderIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus (Id personId) statusList = findAllWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.In statusList]]

findByRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m (Maybe (Id Booking))
findByRiderId (Id personId) = do
  bookings <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamB.riderId $ Se.Eq personId,
            Se.Or
              [ Se.And [Se.Is BeamB.status $ Se.In activeScheduledBookingStatus, Se.Is BeamB.isScheduled $ Se.Eq (Just True)],
                Se.And [Se.Is BeamB.status $ Se.In activeBookingStatus, Se.Is BeamB.isScheduled $ Se.Not $ Se.Eq (Just True)]
              ]
          ]
      ]
  return $ listToMaybe $ Domain.id <$> bookings

findAssignedByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Booking)
findAssignedByRiderId (Id personId) = findOneWithKV [Se.And [Se.Is BeamB.riderId $ Se.Eq personId, Se.Is BeamB.status $ Se.Eq TRIP_ASSIGNED]]

findBookingIdAssignedByEstimateId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Estimate -> [BookingStatus] -> m (Maybe Booking)
findBookingIdAssignedByEstimateId (Id estimateId) statusList = do
  driverOfferIds <- map (Just . getId . (.id)) <$> findAllWithKVAndConditionalDB [Se.Is BeamDO.estimateId $ Se.Eq estimateId] Nothing
  if null driverOfferIds
    then return Nothing
    else do
      quoteIds <- map (Just . getId . (.id)) <$> findAllWithKVAndConditionalDB [Se.Is BeamQ.driverOfferId $ Se.In driverOfferIds] Nothing
      if null quoteIds
        then return Nothing
        else do
          bookings <- findAllWithKVAndConditionalDB [Se.Is BeamB.quoteId $ Se.In quoteIds, Se.Is BeamB.status $ Se.In statusList] Nothing
          return $ listToMaybe bookings

updatePaymentInfo :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Price -> Maybe Price -> Price -> Maybe Text -> m ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare mbPaymentUrl = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.estimatedFare estimatedFare.amount,
      Se.Set BeamB.discount (discount <&> (.amount)),
      Se.Set BeamB.estimatedTotalFare estimatedTotalFare.amount,
      Se.Set BeamB.currency (Just estimatedFare.currency),
      Se.Set BeamB.paymentUrl mbPaymentUrl,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updatePaymentUrl :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Text -> m ()
updatePaymentUrl bookingId paymentUrl = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.paymentUrl (Just paymentUrl),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updatePaymentStatus :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> PaymentStatus -> m ()
updatePaymentStatus rbId paymentStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.paymentStatus (Just paymentStatus),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

-- THIS IS TEMPORARY UNTIL WE HAVE PROPER ADD STOP FEATURE
updateStop :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> Maybe DL.Location -> m ()
updateStop booking mbStopLoc = do
  now <- getCurrentTime
  -- whenJust mbStopLoc $ \stopLoc -> do
  --   void $ whenNothingM_ (QL.findById stopLoc.id) $ QL.create stopLoc
  -- locationMapping <- SLM.buildDropLocationMapping stopLoc.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  -- QLM.create locationMapping

  updateOneWithKV
    [ Se.Set BeamB.stopLocationId (getId . (.id) <$> mbStopLoc),
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
            Se.Is BeamB.fareProductType $ Se.Not $ Se.In [DQuote.RENTAL, DQuote.INTER_CITY],
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

cancelBookings :: (MonadFlow m, EsqDBFlow m r) => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [ Se.Set BeamB.status CANCELLED,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

buildLocation :: (MonadFlow m, EsqDBFlow m r) => DBBL.BookingLocation -> m DL.Location
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

updateMultipleById :: (MonadFlow m, EsqDBFlow m r) => Bool -> HighPrecMoney -> HighPrecMoney -> Maybe Distance -> Id Booking -> m ()
updateMultipleById isBookingUpdated estimatedFare estimatedTotalFare mbEstimatedDistance bookingId = do
  let estimatedDistanceValue = (\estimatedDistance -> distanceToHighPrecDistance estimatedDistance.unit estimatedDistance) <$> mbEstimatedDistance
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.estimatedFare estimatedFare,
      Se.Set BeamB.estimatedTotalFare estimatedTotalFare,
      Se.Set BeamB.estimatedDistance $ distanceToHighPrecMeters <$> mbEstimatedDistance,
      Se.Set BeamB.distance $ distanceToHighPrecMeters <$> mbEstimatedDistance,
      Se.Set BeamB.estimatedDistanceValue estimatedDistanceValue,
      Se.Set BeamB.distanceValue estimatedDistanceValue,
      Se.Set BeamB.isBookingUpdated (Just isBookingUpdated),
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
