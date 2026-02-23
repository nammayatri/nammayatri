module Storage.Queries.BookingExtra where

import Data.List.Extra (notNull)
import qualified Data.Time as DT -- (Day, UTCTime (UTCTime), DT.secondsToDiffTime, utctDay, DT.addDays)
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.Booking
import Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude (forM_, whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude hiding (forM_)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (UTCTime)
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.Booking ()

createBooking' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
createBooking' = createWithKV

createBooking :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Booking -> m ()
createBooking booking = do
  processSingleLocation booking.fromLocation SLM.buildPickUpLocationMapping
  when (notNull booking.stops) $ processMultipleLocations booking.stops
  whenJust booking.toLocation $ \toLocation -> processSingleLocation toLocation SLM.buildDropLocationMapping
  createBooking' booking
  where
    processSingleLocation location locationMappingCreator = do
      locationMap <- locationMappingCreator location.id booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
      QLM.create locationMap
      whenNothingM_ (QL.findById location.id) $ do QL.create location

    processMultipleLocations locations = do
      locationMappings <- SLM.buildStopsLocationMapping locations booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
      QLM.createMany locationMappings
      locations `forM_` \location ->
        whenNothingM_ (QL.findById location.id) $ do QL.create location

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> m (Maybe Booking)
findById (Id bookingId) = findOneWithKV [Se.Is BeamB.id $ Se.Eq bookingId]

findBySTId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DST.SearchTry -> m (Maybe Booking)
findBySTId searchTryId = do
  mbDriverQuote <- QDQuote.findDriverQuoteBySTId searchTryId
  maybe (pure Nothing) (\dQ -> findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq $ getId $ DDQ.id dQ]) mbDriverQuote

findByQuoteId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe Booking)
findByQuoteId quoteId = findOneWithKV [Se.Is BeamB.quoteId $ Se.Eq quoteId]

findByTransactionId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe Booking)
findByTransactionId txnId =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamB.transactionId $ Se.Eq txnId
    ]
    (Just (Se.Desc BeamB.createdAt))
    <&> listToMaybe

findByTransactionIdAndStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> BookingStatus -> m (Maybe Booking)
findByTransactionIdAndStatus txnId status =
  findOneWithKV [Se.And [Se.Is BeamB.transactionId $ Se.Eq txnId, Se.Is BeamB.status $ Se.Eq status]]

findByTransactionIdAndStatuses :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> [BookingStatus] -> m (Maybe Booking)
findByTransactionIdAndStatuses transactionId statusList =
  findAllWithKVAndConditionalDB
    [ Se.Is BeamB.transactionId $ Se.Eq transactionId,
      Se.Is BeamB.status $ Se.In statusList
    ]
    (Just (Se.Desc BeamB.createdAt))
    <&> listToMaybe

findByStatusTripCatSchedulingAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Integer -> Maybe Integer -> Maybe DT.Day -> Maybe DT.Day -> BookingStatus -> Maybe DTC.TripCategory -> [DVST.ServiceTierType] -> Bool -> Id DMOC.MerchantOperatingCity -> Seconds -> m [Booking]
findByStatusTripCatSchedulingAndMerchant mbLimit mbOffset mbFromDay mbToDay status mbTripCategory serviceTiers isScheduled (Id cityId) timeDiffFromUtc = do
  let limitVal = maybe 5 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  now <- getCurrentTime
  (from, to) <- case (mbFromDay, mbToDay) of
    (Just fromDay, Just toDay) -> pure (DT.UTCTime (DT.addDays (-1) fromDay) (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)), DT.UTCTime toDay (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)))
    (Just fromDay, Nothing) -> pure (DT.UTCTime (DT.addDays (-1) fromDay) (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)), now)
    (Nothing, Just toDay) -> pure (DT.UTCTime (DT.fromGregorian 2020 1 1) 0, DT.UTCTime toDay (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)))
    (Nothing, Nothing) -> pure (DT.UTCTime (DT.fromGregorian 2020 1 1) 0, now)
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamB.startTime $ Se.GreaterThanOrEq from,
          Se.Is BeamB.startTime $ Se.LessThanOrEq to,
          Se.Is BeamB.vehicleVariant $ Se.In serviceTiers,
          Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just cityId),
          Se.Is BeamB.isScheduled $ Se.Eq (Just isScheduled),
          Se.Is BeamB.status $ Se.Eq status
        ]
          <> [Se.Is BeamB.tripCategory $ Se.Eq mbTripCategory | isJust mbTripCategory]
    ]
    (Se.Desc BeamB.startTime)
    (Just limitVal)
    (Just offsetVal)

updateStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> BookingStatus -> m ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.status rbStatus, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateStop :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> Maybe Text -> m ()
updateStop bookingId stopLocationId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.stopLocationId stopLocationId,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateStopArrival :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> m ()
updateStopArrival bookingId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.stopLocationId Nothing,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Id RiderDetails -> m ()
updateRiderId rbId riderId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.riderId $ Just $ getId riderId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId rbId)]

updateRiderName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Text -> m ()
updateRiderName bookingId riderName = do
  now <- getCurrentTime
  updateOneWithKV [Se.Set BeamB.riderName $ Just riderName, Se.Set BeamB.updatedAt now] [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateMultipleById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => HighPrecMoney -> Maybe HighPrecMeters -> Maybe Meters -> Text -> Id Booking -> m ()
updateMultipleById estimatedFare maxEstimatedDistance estimatedDistance fareParametersId bookingId = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamB.estimatedFare estimatedFare,
      Se.Set BeamB.maxEstimatedDistance maxEstimatedDistance,
      Se.Set BeamB.estimatedDistance estimatedDistance,
      Se.Set BeamB.fareParametersId fareParametersId,
      Se.Set BeamB.updatedAt now
    ]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSpecialZoneOtpCode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Text -> m ()
updateSpecialZoneOtpCode bookingId specialZoneOtpCode = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.specialZoneOtpCode $ Just specialZoneOtpCode, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

findStuckBookings ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Merchant ->
  DMOC.MerchantOperatingCity ->
  [Id Booking] ->
  UTCTime ->
  m [Id Booking]
findStuckBookings merchant moCity bookingIds now = do
  let updatedTimestamp = addUTCTime (- (6 * 60 * 60)) now
  (Domain.Types.Booking.id <$>)
    <$> findAllWithDb
      [ Se.And
          [ Se.Is BeamB.providerId (Se.Eq merchant.id.getId),
            Se.Is BeamB.id (Se.In (getId <$> bookingIds)),
            Se.Is BeamB.status (Se.In [NEW, TRIP_ASSIGNED]),
            Se.Is BeamB.createdAt (Se.LessThanOrEq updatedTimestamp),
            Se.Is BeamB.tripCategory $ Se.In [Nothing, Just (DTC.OneWay DTC.OneWayOnDemandDynamicOffer), Just (DTC.OneWay DTC.OneWayOnDemandStaticOffer), Just (DTC.OneWay DTC.OneWayRideOtp)],
            Se.Or
              ( [Se.Is BeamB.merchantOperatingCityId $ Se.Eq $ Just (getId moCity.id)]
                  <> [Se.Is BeamB.merchantOperatingCityId $ Se.Eq Nothing | moCity.city == merchant.city]
              )
          ]
      ]

findBookingBySpecialZoneOTPAndCity :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> UTCTime -> Int -> m (Maybe Booking)
findBookingBySpecialZoneOTPAndCity cityId otpCode now specialZoneBookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (- (fromIntegral specialZoneBookingOtpExpiry * 60) :: NominalDiffTime) now
  findOneWithKVRedis [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just cityId), Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

findBookingBySpecialZoneOTP :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> UTCTime -> Int -> m (Maybe Booking)
findBookingBySpecialZoneOTP otpCode now specialZoneBookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (- (fromIntegral specialZoneBookingOtpExpiry * 60) :: NominalDiffTime) now
  findOneWithKVRedis [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

cancelBookings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Booking] -> UTCTime -> m ()
cancelBookings bookingIds now =
  updateWithKV
    [Se.Set BeamB.status CANCELLED, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.In $ getId <$> bookingIds)]

findFareForCancelledBookings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Id Booking] -> m HighPrecMoney
findFareForCancelledBookings bookingIds = findAllWithKV [Se.And [Se.Is BeamB.status $ Se.Eq CANCELLED, Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]] <&> sum . map Domain.Types.Booking.estimatedFare

findLastCancelledByRiderId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id RiderDetails -> m (Maybe Booking)
findLastCancelledByRiderId riderDetailsId =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is BeamB.riderId (Se.Eq (Just riderDetailsId.getId)),
          Se.Is BeamB.status (Se.Eq CANCELLED)
        ]
    ]
    (Se.Desc BeamB.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe

updatePaymentId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Text -> m ()
updatePaymentId bookingId paymentId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.paymentId $ Just paymentId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

findBookingsFromDB :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Id Booking] -> m [Booking]
findBookingsFromDB bookingIds = findAllWithKV [Se.Is BeamB.id $ Se.In (getId <$> bookingIds)]

findByIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id Booking] ->
  m [Booking]
findByIds bookingIds = do
  findAllWithKV [Se.Is BeamB.id $ Se.In $ getId <$> bookingIds]

updateExotelCallDeclinedTime :: (MonadFlow m, EsqDBFlow m r) => Id Booking -> m ()
updateExotelCallDeclinedTime bookingId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.exotelDeclinedCallStatusReceivingTime $ Just now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateSearchTryId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Id DST.SearchTry -> m ()
updateSearchTryId bookingId searchTryId = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.searchTryId $ Just $ getId searchTryId, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]

updateDqDurationToPickup :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Booking -> Seconds -> m ()
updateDqDurationToPickup bookingId durationToPickup = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamB.dqDurationToPickup $ Just durationToPickup, Se.Set BeamB.updatedAt now]
    [Se.Is BeamB.id (Se.Eq $ getId bookingId)]
