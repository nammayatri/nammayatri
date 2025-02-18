module Storage.Queries.BookingExtra where

import Control.Applicative
import Data.List.Extra (notNull)
import qualified Database.Beam as B
import Domain.Types
import Domain.Types.Booking as Domain
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingLocation as DBBL
import qualified Domain.Types.FarePolicy.FareProductType as DQuote
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import EulerHS.Prelude (forM_, whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude hiding (forM_)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Queries.BookingLocation as QBBL
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.DriverOffer ()
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.Booking ()
import qualified Storage.Queries.Quote ()

createBooking' :: (MonadFlow m, EsqDBFlow m r) => Booking -> m ()
createBooking' = createWithKV

createBooking :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
createBooking booking = do
  processSingleLocation booking.fromLocation SLM.buildPickUpLocationMapping
  (mbToLocation, stops) <-
    case booking.bookingDetails of
      DRB.OneWayDetails detail -> return (Just detail.toLocation, detail.stops)
      DRB.RentalDetails detail -> return (detail.stopLocation, [])
      DRB.DriverOfferDetails detail -> return (Just detail.toLocation, detail.stops)
      DRB.OneWaySpecialZoneDetails detail -> return (Just detail.toLocation, detail.stops)
      DRB.InterCityDetails detail -> return (Just detail.toLocation, [])
      DRB.AmbulanceDetails detail -> return (Just detail.toLocation, [])
      DRB.DeliveryDetails detail -> return (Just detail.toLocation, [])
  when (notNull stops) $ processMultipleLocations stops
  whenJust mbToLocation $ \toLocation -> processSingleLocation toLocation SLM.buildDropLocationMapping
  createBooking' booking
  where
    processSingleLocation location locationMappingCreator = do
      locationMap <- locationMappingCreator location.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
      QLM.create locationMap
      whenNothingM_ (QL.findById location.id) $ do QL.create location

    processMultipleLocations locations = do
      locationMappings <- SLM.buildStopsLocationMapping locations booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
      QLM.createMany locationMappings
      locations `forM_` \location ->
        whenNothingM_ (QL.findById location.id) $ do QL.create location

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
  dbConf <- getReplicaBeamConfig
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
  dbConf <- getReplicaBeamConfig
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

findByTransactionIdAndStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> [BookingStatus] -> m (Maybe Booking)
findByTransactionIdAndStatus transactionId statusList =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamB.riderTransactionId $ Se.Eq transactionId,
      Se.Is BeamB.status $ Se.In statusList
    ]
    (Just (Se.Desc BeamB.createdAt))
    <&> listToMaybe

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
updateStop :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> Maybe DL.Location -> Maybe Bool -> m ()
updateStop booking mbStopLoc isBookingUpdated = do
  now <- getCurrentTime
  -- whenJust mbStopLoc $ \stopLoc -> do
  --   void $ whenNothingM_ (QL.findById stopLoc.id) $ QL.create stopLoc
  -- locationMapping <- SLM.buildDropLocationMapping stopLoc.id booking.id.getId DLM.BOOKING (Just booking.merchantId) (Just booking.merchantOperatingCityId)
  -- QLM.create locationMapping

  updateOneWithKV
    ( [ Se.Set BeamB.stopLocationId (getId . (.id) <$> mbStopLoc),
        Se.Set BeamB.updatedAt now
      ]
        <> [Se.Set BeamB.isBookingUpdated isBookingUpdated | isJust isBookingUpdated]
    )
    [Se.Is BeamB.id (Se.Eq $ getId booking.id)]

findAllByPersonIdLimitOffset :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Maybe Integer -> Maybe Integer -> m [Booking]
findAllByPersonIdLimitOffset (Id personId) mlimit moffset = do
  let limit' = fmap fromIntegral $ mlimit <|> Just 100
      offset' = fmap fromIntegral $ moffset <|> Just 0
  findAllWithOptionsKV [Se.Is BeamB.riderId $ Se.Eq personId] (Se.Desc BeamB.createdAt) limit' offset'

findStuckBookings :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Merchant -> DMOC.MerchantOperatingCity -> [Id Booking] -> UTCTime -> m [Booking]
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

buildLocation ::
  (MonadFlow m, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  DBBL.BookingLocation ->
  m DL.Location
buildLocation merchantId merchantOperatingCityId DBBL.BookingLocation {..} =
  return $
    DL.Location
      { id = cast id,
        merchantId = Just merchantId,
        ..
      }

upsertFromLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe (Id DBBL.BookingLocation) -> Text -> Text -> Maybe Text -> m DL.Location
upsertFromLocationAndMappingForOldData locationId bookingId merchantId merchantOperatingCityId = do
  loc <- QBBL.findById `mapM` locationId >>= fromMaybeM (InternalError "From Location Id Not Found in Booking Table")
  pickupLoc <- maybe (throwError $ InternalError ("From Location Not Found in Booking Location Table for BookingId : " <> bookingId)) (buildLocation (Id merchantId) (Id <$> merchantOperatingCityId)) loc
  fromLocationMapping <- SLM.buildPickUpLocationMapping pickupLoc.id bookingId DLM.BOOKING (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  void $ QL.create pickupLoc >> QLM.create fromLocationMapping
  return pickupLoc

upsertToLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Text -> Text -> Text -> Maybe Text -> m ()
upsertToLocationAndMappingForOldData toLocationId bookingId merchantId merchantOperatingCityId = do
  toLocation <- maybe (pure Nothing) (QBBL.findById . Id) toLocationId >>= fromMaybeM (InternalError "toLocation is null for one way booking")
  dropLoc <- buildLocation (Id merchantId) (Id <$> merchantOperatingCityId) toLocation
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

findAllByTransactionId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m [Booking]
findAllByTransactionId transactionId = findAllWithKVAndConditionalDB [Se.Is BeamB.riderTransactionId $ Se.Eq transactionId] (Just (Se.Desc BeamB.createdAt))

updateIsCancelled :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Maybe Bool -> m ()
updateIsCancelled (Id reqId) isDeleted = do
  updateOneWithKV
    [Se.Set BeamB.isDeleted isDeleted]
    [Se.Is BeamB.id (Se.Eq reqId)]

updateisSkipped :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Maybe Bool -> m ()
updateisSkipped (Id reqId) isSkipped = do
  updateOneWithKV
    [Se.Set BeamB.isSkipped isSkipped]
    [Se.Is BeamB.id (Se.Eq reqId)]
