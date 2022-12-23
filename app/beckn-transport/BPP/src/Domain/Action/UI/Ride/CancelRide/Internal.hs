module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as SDrLoc
import SharedLogic.DriverPool
import qualified SharedLogic.Ride as SRide
import SharedLogic.TransporterConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideRequest as RideRequest
import Tools.Error
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

cancelRideImpl ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasDriverPoolConfig r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideImpl rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId = booking.providerId
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  if isCancelledByDriver
    then reallocateRideTransaction transporter.subscriberId booking.id ride bookingCReason
    else cancelRideTransaction booking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    if isCancelledByDriver
      then BP.sendBookingReallocationUpdateToBAP booking ride.id transporter bookingCReason.source
      else BP.sendBookingCancelledUpdateToBAP booking transporter bookingCReason.source
  fork "cancelRide - Notify driver" $ do
    driver <- Person.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    fcmConfig <- findFCMConfigByMerchantId transporterId
    Notify.notifyOnCancel fcmConfig booking driver.id driver.deviceToken bookingCReason.source
  where
    isCancelledByDriver = bookingCReason.source == SBCR.ByDriver

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id SRB.Booking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideTransaction bookingId ride bookingCReason = do
  Esq.runTransaction $ do
    updateDriverInfo ride.driverId
    QRide.updateStatus ride.id SRide.CANCELLED
    QRB.updateStatus bookingId SRB.CANCELLED
    QBCR.upsert bookingCReason
  SRide.clearCache $ cast ride.driverId
  SDrLoc.clearDriverInfoCache $ cast ride.driverId
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

reallocateRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  ShortId DM.Subscriber ->
  Id SRB.Booking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
reallocateRideTransaction subscriberId bookingId ride bookingCReason = do
  rideRequest <- buildRideReq
  Esq.runTransaction $ do
    QRB.updateStatus bookingId SRB.AWAITING_REASSIGNMENT
    QRB.increaseReallocationsCounter bookingId
    QRide.updateStatus ride.id SRide.CANCELLED
    updateDriverInfo
    QBCR.upsert bookingCReason
    RideRequest.create rideRequest
  SRide.clearCache $ cast ride.driverId
  SDrLoc.clearDriverInfoCache $ cast ride.driverId
  where
    updateDriverInfo = do
      let driverId = cast ride.driverId
      DriverInformation.updateOnRide driverId False
      QDriverStats.updateIdleTime driverId
    buildRideReq = do
      guid <- generateGUID
      now <- getCurrentTime
      pure
        SRideRequest.RideRequest
          { id = Id guid,
            bookingId = bookingId,
            subscriberId = subscriberId,
            createdAt = now,
            _type = SRideRequest.ALLOCATION,
            info = Nothing
          }
