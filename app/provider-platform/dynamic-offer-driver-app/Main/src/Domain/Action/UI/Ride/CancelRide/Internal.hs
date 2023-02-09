module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.DriverInformation as CDI
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

cancelRideImpl ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EncFlow m r,
    HedisFlow m r,
    HasHttpClientOptions r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
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
  cancelRideTransaction booking.id ride bookingCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  fork "cancelRide - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCReason.source
  fork "cancelRide - Notify driver" $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    when (bookingCReason.source == SBCR.ByDriver) $
      DP.incrementCancellationCount transporterId driver.id
    Notify.notifyOnCancel transporterId booking driver.id driver.deviceToken bookingCReason.source

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Id SRB.Booking ->
  SRide.Ride ->
  SBCR.BookingCancellationReason ->
  m ()
cancelRideTransaction bookingId ride bookingCReason = do
  let driverId = cast ride.driverId
  DLoc.updateOnRide driverId False
  driverInfo <- CDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  Esq.runTransaction $ do
    when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
    QRide.updateStatus ride.id SRide.CANCELLED
    QRB.updateStatus bookingId SRB.CANCELLED
    QBCR.upsert bookingCReason
    if driverInfo.active
      then QDFS.updateStatus ride.driverId DDFS.ACTIVE
      else QDFS.updateStatus ride.driverId DDFS.IDLE
  SRide.clearCache ride.driverId
