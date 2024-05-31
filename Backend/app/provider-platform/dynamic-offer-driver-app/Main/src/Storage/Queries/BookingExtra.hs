{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingExtra where

import qualified Data.Text as T
import qualified Data.Time as DT -- (Day, UTCTime (UTCTime), DT.secondsToDiffTime, utctDay, DT.addDays)
import Domain.Types.Booking
import qualified Domain.Types.BookingLocation as DBBL
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.RiderDetails (RiderDetails)
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (UTCTime)
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.Booking as BeamB
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.BookingLocation as QBBL
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.FareParameters as QueriesFP
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.Booking
import Tools.Error

createBooking' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
createBooking' = createWithKV

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
create dBooking = do
  void $ whenNothingM_ (QL.findById dBooking.fromLocation.id) $ do QL.create dBooking.fromLocation
  whenJust dBooking.toLocation $ \toLocation -> whenNothingM_ (QL.findById toLocation.id) $ do QL.create toLocation
  createBooking' dBooking

createBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Booking -> m ()
createBooking booking = do
  fromLocationMap <- SLM.buildPickUpLocationMapping booking.fromLocation.id booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
  QLM.create fromLocationMap
  whenJust booking.toLocation $ \toLocation -> do
    toLocationMaps <- SLM.buildDropLocationMapping toLocation.id booking.id.getId DLM.BOOKING (Just booking.providerId) (Just booking.merchantOperatingCityId)
    QLM.create toLocationMaps
  create booking

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

findByStatusTripCatSchedulingAndMerchant :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Integer -> Maybe Integer -> Maybe DT.Day -> BookingStatus -> Maybe DTC.TripCategory -> [DVST.ServiceTierType] -> Bool -> Id DMOC.MerchantOperatingCity -> Seconds -> m [Booking]
findByStatusTripCatSchedulingAndMerchant mbLimit mbOffset mbDay status mbTripCategory serviceTiers isScheduled (Id merchanOperatingCityId) timeDiffFromUtc = do
  let limitVal = maybe 5 fromInteger mbLimit
      offsetVal = maybe 0 fromInteger mbOffset
  (from, to) <- case mbDay of
    Just day -> pure (DT.UTCTime (DT.addDays (-1) day) (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)), DT.UTCTime day (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)))
    Nothing -> do
      now <- getLocalCurrentTime timeDiffFromUtc
      let day_ = DT.utctDay now
      pure (DT.UTCTime (DT.addDays (-1) day_) (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)), DT.UTCTime day_ (86400 - DT.secondsToDiffTime (toInteger timeDiffFromUtc.getSeconds)))
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is BeamB.startTime $ Se.GreaterThanOrEq from,
          Se.Is BeamB.startTime $ Se.LessThanOrEq to,
          Se.Is BeamB.vehicleVariant $ Se.In serviceTiers,
          Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just merchanOperatingCityId),
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

findBookingBySpecialZoneOTP :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Text -> UTCTime -> Int -> m (Maybe Booking)
findBookingBySpecialZoneOTP cityId otpCode now specialZoneBookingOtpExpiry = do
  let otpExpiryCondition = addUTCTime (- (fromIntegral specialZoneBookingOtpExpiry * 60) :: NominalDiffTime) now
  findOneWithKV [Se.And [Se.Is BeamB.specialZoneOtpCode $ Se.Eq (Just otpCode), Se.Is BeamB.merchantOperatingCityId $ Se.Eq (Just cityId), Se.Is BeamB.createdAt $ Se.GreaterThanOrEq otpExpiryCondition, Se.Is BeamB.status $ Se.Eq NEW]]

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
