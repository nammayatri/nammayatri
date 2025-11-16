module SharedLogic.Booking where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import Data.Text hiding (map)
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.SharedLogic.Cancel as SharedCancel
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as SRide
import Domain.Types.Trip
import Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Ride
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import Text.Printf (printf)
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    MonadCatch m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c
  ) =>
  DRB.Booking ->
  Maybe DPerson.Person ->
  DM.Merchant ->
  m ()
cancelBooking booking mbDriver transporter = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  let transporterId' = Just booking.providerId
  unless (transporterId' == Just transporter.id) $ throwError AccessDenied
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- case mbDriver of
    Nothing -> buildBookingCancellationReason Nothing mbRide transporterId'
    Just driver -> buildBookingCancellationReason (Just driver.id) mbRide transporterId'
  transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)

  -- Lock Description: This is a Shared Lock held Between Booking Cancel for Customer & Driver, At a time only one of them can do the full Cancel to OnCancel/Reallocation flow.
  -- Lock Release: Held for 30 seconds and released at the end of the OnCancel.
  SharedCancel.tryCancellationLock booking.transactionId $ do
    when (fromMaybe False transporter.prepaidSubscriptionAndWalletEnabled && isJust transporterConfig.subscriptionConfig.fleetPrepaidSubscriptionThreshold) $ whenJust mbRide $ \ride -> releaseLien booking ride
    whenJust mbDriver $ \driver ->
      updateOnRideStatusWithAdvancedRideCheck driver.id mbRide
    QRB.updateStatus booking.id DRB.CANCELLED
    when booking.isScheduled $ removeBookingFromRedis booking
    QBCR.upsert bookingCancellationReason
    whenJust mbRide $ \ride -> do
      void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
      QRide.updateStatus ride.id SRide.CANCELLED
      updateOnRideStatusWithAdvancedRideCheck (cast ride.driverId) mbRide
      void $ LF.rideDetails ride.id SRide.CANCELLED transporter.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))

    fork "cancelBooking - Notify BAP" $ do
      BP.sendBookingCancelledUpdateToBAP booking transporter bookingCancellationReason.source Nothing
    whenJust mbRide $ \ride ->
      case mbDriver of
        Nothing -> throwError (PersonNotFound ride.driverId.getId)
        Just driver -> do
          fork "cancelRide - Notify driver" $ do
            Notify.notifyOnCancel booking.merchantOperatingCityId booking driver bookingCancellationReason.source
  where
    buildBookingCancellationReason driverId ride merchantId = do
      return $
        DBCR.BookingCancellationReason
          { driverId = driverId,
            bookingId = booking.id,
            rideId = (.id) <$> ride,
            merchantId = merchantId,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            distanceUnit = booking.distanceUnit,
            merchantOperatingCityId = Just booking.merchantOperatingCityId
          }

removeBookingFromRedis :: (MonadFlow m, CacheFlow m r) => DRB.Booking -> m ()
removeBookingFromRedis booking = do
  let vehicleVariant = castServiceTierToVariant booking.vehicleServiceTier
  let redisKey = createRedisKey booking.startTime booking.merchantOperatingCityId booking.tripCategory vehicleVariant
      redisKeyForHset = createRedisKeyForHset booking.startTime booking.merchantOperatingCityId
      member = createMember booking
  void $ Redis.zRem redisKey [member]
  void $ Redis.hDel redisKeyForHset [booking.id.getId]

createMember :: DRB.Booking -> Text
createMember booking = booking.id.getId <> "|" <> (pack . show $ booking.fromLocation.lat) <> "|" <> (pack . show $ booking.fromLocation.lon) <> "|" <> pack (formatTime defaultTimeLocale "%FT%T%z" booking.startTime) <> "|" <> (pack . show $ booking.vehicleServiceTier)

createRedisKey :: UTCTime -> Id MerchantOperatingCity -> TripCategory -> VehicleVariant -> Text
createRedisKey time mocId tripCategory vehicleVariant =
  let (year, month, day) = toGregorian $ utctDay time
      date = printf "%04d-%02d-%02d" year month day
      cityId = unpack mocId.getId
      tripType = createTripType tripCategory
   in (pack $ "ScheduledBookings:" <> cityId <> ":" <> date <> ":" <> show vehicleVariant <> ":" <> tripType)

createTripType :: TripCategory -> [Char]
createTripType (OneWay _) = "OneWay"
createTripType (Rental _) = "Rental"
createTripType (RideShare _) = "RideShare"
createTripType (InterCity _ _) = "InterCity"
createTripType (CrossCity _ _) = "CrossCity"
createTripType (Ambulance _) = "Ambulance"
createTripType (Delivery _) = "Delivery"

createRedisKeyForHset :: UTCTime -> Id MerchantOperatingCity -> Text
createRedisKeyForHset time mocId =
  let (year, month, day) = toGregorian $ utctDay time
      date = printf "%04d-%02d-%02d" year month day
      cityId = unpack mocId.getId
   in (pack $ "ScheduledBookings:" <> cityId <> ":" <> date)

calculateSortedSetScore :: UTCTime -> Double
calculateSortedSetScore time = realToFrac (utcTimeToPOSIXSeconds time)

createRedisKeysForCombinations :: UTCTime -> Id MerchantOperatingCity -> [TripCategory] -> [VehicleVariant] -> [Text]
createRedisKeysForCombinations time mocId tripCategories vehicleVariants =
  [ createRedisKey time mocId tripCategory vehicleVariant
    | tripCategory <- tripCategories,
      vehicleVariant <- vehicleVariants
  ]

addScheduledBookingInRedis :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DRB.Booking -> m ()
addScheduledBookingInRedis booking = do
  let score = ceiling $ calculateSortedSetScore booking.startTime
      expirationSeconds = ceiling $ 86400 - utctDayTime booking.startTime
      vehicleVariant = castServiceTierToVariant booking.vehicleServiceTier
      redisKey = createRedisKey booking.startTime booking.merchantOperatingCityId booking.tripCategory vehicleVariant
      redisKeyForHset = createRedisKeyForHset booking.startTime booking.merchantOperatingCityId
      member = createMember booking
  void $ Redis.zAddExp redisKey member score expirationSeconds
  void $ Redis.hSetExp redisKeyForHset booking.id.getId booking expirationSeconds
