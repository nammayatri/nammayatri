module SharedLogic.Booking where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import Data.Text
import Data.Time (UTCTime (..), addDays, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
-- import Domain.Types.ServiceTierType
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as SRide
import Domain.Types.VehicleCategory
import Domain.Types.VehicleVariant
import Kernel.External.Encryption
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
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    LT.HasLocationService m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
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
    Just driver -> do
      updateOnRideStatusWithAdvancedRideCheck driver.id mbRide
      buildBookingCancellationReason (Just driver.id) mbRide transporterId'

  QRB.updateStatus booking.id DRB.CANCELLED
  void $ removeBookingFromRedis booking
  QBCR.upsert bookingCancellationReason
  whenJust mbRide $ \ride -> do
    void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
    QRide.updateStatus ride.id SRide.CANCELLED
    updateOnRideStatusWithAdvancedRideCheck (cast ride.driverId) mbRide
    void $ LF.rideDetails ride.id SRide.CANCELLED transporter.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon

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
            distanceUnit = booking.distanceUnit
          }

removeBookingFromRedis :: (MonadFlow m, CacheFlow m r) => DRB.Booking -> m ()
removeBookingFromRedis booking = do
  let vehicleCategory = castServiceTierToVehicleCategory booking.vehicleServiceTier
  let redisKey = createRedisKey booking.startTime booking.merchantOperatingCityId vehicleCategory
      redisKeyForHset = createRedisKeyForHset booking.startTime booking.merchantOperatingCityId
  logDebug $ "redisKeyAtT-30 : " <> show redisKey
  void $ Redis.zRem redisKey [booking.id.getId]
  void $ Redis.hDel redisKeyForHset [booking.id.getId]

createRedisKey :: UTCTime -> Id MerchantOperatingCity -> VehicleCategory -> Text
createRedisKey time mocId vehicleCategory =
  let (year, month, day) = toGregorian $ utctDay time
      date = printf "%04d-%02d-%02d" year month day
      cityId = unpack mocId.getId
   in (pack $ "ScheduledBookings:" <> cityId <> ":" <> date <> ":" <> show vehicleCategory)

createRedisKeyForHset :: UTCTime -> Id MerchantOperatingCity -> Text
createRedisKeyForHset time mocId =
  let (year, month, day) = toGregorian $ utctDay time
      date = printf "%04d-%02d-%02d" year month day
      cityId = unpack mocId.getId
   in (pack $ "ScheduledBookings:" <> cityId <> ":" <> date)

calculateSortedSetScore :: UTCTime -> Double
calculateSortedSetScore time = realToFrac (utcTimeToPOSIXSeconds time)

secondsRemainingInDay :: UTCTime -> Int
secondsRemainingInDay currentTime =
  let currentDay = utctDay currentTime
      nextDay = addDays 1 currentDay
      dayEnd = UTCTime nextDay 0
      secondsRemaining = diffUTCTime dayEnd currentTime
   in ceiling secondsRemaining
